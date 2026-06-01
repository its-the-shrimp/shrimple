use {
    crate::{
        asset::AssetManager,
        ast::{AstBuilder, Attr, XmlElement, XmlNode, XmlText},
        error::{Expansions, ExtraCtx, collect_template_expansion_info},
        parser::XmlTextFragment,
        utils::{InlineStr, Result, default},
        view::StrView,
    },
    anyhow::{Context, anyhow, bail, ensure},
    mlua::{FromLua, Integer, Lua, Value},
    shrimple_parser::{
        Input,
        utils::{FullLocation, PathLike},
    },
    std::{
        fmt::Write,
        fs::read_dir,
        iter::chain,
        mem::{replace, take},
        ops::Not,
        path::Path,
        sync::Arc,
    },
};

/// Intentionally contains symbols that an HTML element cannot have, so that this name identifies
/// only the actual root element created by the compiler
const ROOT_ELEMEMT: &str = "\n<ROOT>\n";

struct LuaCtx {
    inner: Lua,
}

impl Default for LuaCtx {
    fn default() -> Self {
        Self { inner: default() }
    }
}

impl LuaCtx {
    fn with_var<R>(&self, name: &str, f: impl FnOnce(&str) -> R) -> mlua::Result<R> {
        let str = self.inner.globals().raw_get::<&str, Option<mlua::String>>(name)?;
        Ok(match str {
            Some(s) => f(s.to_str()?),
            None => f(""),
        })
    }

    fn set_var(&self, name: &str, value: &str) -> mlua::Result<()> {
        self.inner.globals().set(name, value)
    }

    fn remove_var(&self, name: &str) -> mlua::Result<()> {
        self.inner.globals().set(name, Value::Nil)
    }

    /// if the variable is `nil`, it's set to 0
    fn increment_var(&self, name: &str) -> Result {
        let globals = self.inner.globals();
        globals.set(
            name,
            match globals.raw_get(name)? {
                Value::Nil => 0,
                val => Integer::from_lua(val, &self.inner)?
                    .checked_add(1)
                    .with_context(|| format!("integer overflow while incrementing `{name}`"))?,
            },
        )?;
        Ok(())
    }

    fn with_eval_result<R>(&self, code: &str, f: impl FnOnce(&str) -> R) -> mlua::Result<R> {
        Ok(match self.inner.load(code).set_name("").eval()? {
            Value::Nil => f(""),

            Value::String(s) => f(s.to_str()?),

            Value::Integer(i) => {
                let mut str = InlineStr::<64>::default();
                _ = write!(str, "{i}");
                f(&str)
            }

            Value::Number(n) => {
                let mut str = InlineStr::<64>::default();
                _ = write!(str, "{n}");
                f(&str)
            }

            other => f(&String::from_lua(other, &self.inner)?),
        })
    }
}

struct Param {
    name: StrView,
    default: Option<StrView>,
}

struct Template {
    name: StrView,
    accepts_children: bool,
    /// Sorted by name
    params: Box<[Param]>,
    body: Box<[XmlNode]>,
}

impl Template {
    fn get_param(&self, name: &str) -> Result<&Param> {
        let Ok(param_id) = self.params.binary_search_by_key(&name, |param| &param.name) else {
            bail!("template `{}` has no parameter named `{}`", self.name, name);
        };
        Ok(&self.params[param_id])
    }
}

struct TemplateExpansion {
    template_name_in_invoc: StrView,
    body: Box<[XmlNode]>,
}

struct Compiler {
    is_top_level: bool,
    asset_manager: AssetManager,
    lua_ctx: LuaCtx,
    // wrapped in Arc to prevent borrow checking errors
    templates: Vec<Arc<Template>>,
    template_expansions: Vec<TemplateExpansion>,
    /// The piece of source code the compiler was processing when the error occured
    error_ctx_frag: Option<StrView>,
}

impl Compiler {
    fn new() -> Self {
        Self {
            is_top_level: false,
            asset_manager: default(),
            lua_ctx: default(),
            templates: default(),
            template_expansions: default(),
            error_ctx_frag: default(),
        }
    }

    fn compile_text_fragment(
        &mut self,
        fragment: &XmlTextFragment,
        dst: &mut String,
    ) -> Result<()> {
        match fragment {
            XmlTextFragment::Var(name) => {
                self.error_ctx_frag = Some(name.clone());
                self.lua_ctx.with_var(name, |v| dst.write_str(v))??;
            }
            XmlTextFragment::Expr(code) => {
                self.error_ctx_frag = Some(code.clone());
                self.lua_ctx.with_eval_result(code, |res| dst.write_str(res))??;
            }
            XmlTextFragment::Text(text) => dst.write_str(text)?,
        }

        Ok(())
    }

    fn compile_text_node(&mut self, node: &mut XmlText) -> Result {
        if node.parts.is_empty() {
            return Ok(());
        }

        let mut res_md = String::new();
        for part in take(&mut node.parts) {
            self.compile_text_fragment(&part, &mut res_md).inspect_err(|_| {
                self.error_ctx_frag = Some(part.into_text());
            })?;
        }
        // TODO: Markdown to HTML
        node.parts = [XmlTextFragment::Text(res_md.into())].into();
        Ok(())
    }

    fn get_required_compiled_attr_value<'dst>(
        &mut self,
        name: &StrView,
        mut src: XmlText,
        dst: &'dst mut Option<StrView>,
    ) -> Result<&'dst mut StrView> {
        ensure!(dst.is_none(), "`{name}` attribute is already provided");

        self.compile_text_node(&mut src)?;
        Ok(dst.insert(
            src.parts
                .into_iter()
                .next()
                .with_context(|| {
                    self.error_ctx_frag = Some(name.clone());
                    format!("`{name}` attribute must have a value")
                })?
                .into_text(),
        ))
    }

    fn get_optional_compiled_attr_value(
        &mut self,
        name: &StrView,
        mut src: XmlText,
        dst: &mut Option<StrView>,
    ) -> Result {
        ensure!(dst.is_none(), "`{name}` attribute is already provided");

        self.compile_text_node(&mut src)?;
        self.error_ctx_frag = Some(name.clone());
        *dst = src.parts.into_iter().next().map(XmlTextFragment::into_text);
        Ok(())
    }

    // TODO: come up with a way to control the value of boolean flags via Lua.
    // Which string values do we treat as `true`? which as `false`? should any other values cause
    // an error?
    fn get_flag_value(&mut self, attr: &Attr) -> Result<bool> {
        self.error_ctx_frag = Some(attr.name.clone());
        match &*attr.value.parts {
            [] => Ok(true),
            _ => bail!("`{}` attribute is just a flag and must not have a value", attr.name),
        }
    }

    fn get_template(&self, name: &str) -> Result<Arc<Template>> {
        let Ok(template_id) = self.templates.binary_search_by_key(&name, |t| &t.name) else {
            bail!("template `{name}` is not defined");
        };
        Ok(self.templates[template_id].clone())
    }

    fn forbid_children_in_element(&mut self, element: &XmlElement) -> Result {
        if !element.body.is_empty() {
            self.error_ctx_frag = Some(element.name.clone());
            bail!("`{}` element cannot have children", element.name)
        }
        Ok(())
    }

    fn compile_template_element(&mut self, element: &mut XmlElement) -> Result {
        self.error_ctx_frag = Some(take(&mut element.name));
        if !self.is_top_level {
            bail!("`<$template>` is only allowed at the top level");
        }

        let mut name = None;
        let mut accepts_children = false;
        let mut params = vec![];

        for attr in take(&mut element.attrs) {
            self.error_ctx_frag = Some(attr.name.clone());
            match &*attr.name {
                "name" => {
                    self.get_required_compiled_attr_value(&attr.name, attr.value, &mut name)?;
                    if name.as_deref().is_none_or(str::is_empty) {
                        bail!("template name must be provided");
                    }
                }
                "acceptsChildren" => accepts_children = self.get_flag_value(&attr)?,
                param if param.starts_with('$') => {
                    let mut default = None;
                    self.get_optional_compiled_attr_value(&attr.name, attr.value, &mut default)?;
                    let name = attr.name.after(1);
                    params.push(Param { name, default });
                }
                unknown => bail!("`$template` has no attribute named `{unknown}`"),
            }
        }

        let Some(name) = name else {
            bail!("template name must be provided");
        };

        let Err(new_template_id) = self.templates.binary_search_by_key(&&name, |t| &t.name) else {
            let err = anyhow!("template `{name}` is already defined");
            self.error_ctx_frag = Some(name);
            return Err(err);
        };
        self.templates.insert(
            new_template_id,
            Arc::new(Template {
                name,
                accepts_children,
                params: params.into(),
                body: take(&mut element.body),
            }),
        );
        Ok(())
    }

    // TODO: consider expanding `<$children />` to an ad-hoc template to not reduce copying
    fn expand_template(&mut self, element: &mut XmlElement) -> Result {
        self.error_ctx_frag = Some(element.name.clone());
        let name = take(&mut element.name).after(1);
        let template = self.get_template(&name)?;

        if !template.accepts_children {
            self.forbid_children_in_element(element)?;
        }
        self.template_expansions.push(TemplateExpansion {
            body: replace(&mut element.body, template.body.clone()),
            template_name_in_invoc: name,
        });

        for param in &template.params {
            if element.attrs.iter().any(|arg| arg.name == param.name) {
                continue;
            }

            self.error_ctx_frag = Some(param.name.clone());
            let Some(default) = param.default.as_ref() else {
                bail!("no value provided for parameter `{}`", param.name);
            };
            self.lua_ctx.set_var(&param.name, default)?;
        }

        for arg in take(&mut element.attrs) {
            self.error_ctx_frag = Some(arg.name.clone());
            template.get_param(&arg.name)?;

            let mut value = None;
            self.get_required_compiled_attr_value(&arg.name, arg.value, &mut value)?;
            self.lua_ctx.set_var(&arg.name, &value.context("bug: required value not set")?)?;
        }

        for node in &mut element.body {
            self.compile_node(node)?;
        }

        self.template_expansions.pop();
        Ok(())
    }

    fn compile_foreach_element(&mut self, element: &mut XmlElement) -> Result {
        let mut attrs_iter = take(&mut element.attrs).into_iter();
        self.error_ctx_frag = Some(take(&mut element.name));

        let Some(iter_var_name_attr) = attrs_iter.next() else {
            bail!("expected the iterator variable declaration, got nothing");
        };
        if !iter_var_name_attr.name.starts_with('$') {
            self.error_ctx_frag = Some(iter_var_name_attr.name);
            bail!(
                "expected the iterator variable declaration. \
                           Prefix the variable name with `$`"
            );
        }
        if let Some(value_part) = iter_var_name_attr.value.parts.first() {
            self.error_ctx_frag = Some(value_part.as_text().clone());
            bail!(
                "the iterator variable cannot have a pre-defined value. \
                            Remove `=` and the value after it"
            );
        }
        let iter_var_name = iter_var_name_attr.name.after(1);

        let Some(in_kw) = attrs_iter.next() else {
            let iter_var_name_len = iter_var_name.len();
            self.error_ctx_frag = Some(iter_var_name.after(iter_var_name_len));
            bail!("expected `in`, got nothing");
        };
        if in_kw.name != "in" {
            let e = anyhow!("expected `in`, got `{}`", in_kw.name);
            self.error_ctx_frag = Some(in_kw.name);
            return Err(e);
        }
        if let Some(value_part) = in_kw.value.parts.first() {
            self.error_ctx_frag = Some(value_part.as_text().clone());
            bail!("extraneous text starting from `=`. Remove it");
        }

        let Some(dir_attr) = attrs_iter.next() else {
            let in_kw_len = in_kw.name.len();
            self.error_ctx_frag = Some(in_kw.name.after(in_kw_len));
            bail!("expected the `dir` iterator, found nothing");
        };
        self.error_ctx_frag = Some(dir_attr.name.clone());
        ensure!(dir_attr.name == "dir", "expected `dir`, got `{}`", dir_attr.name);
        let mut dir = None;
        let dir =
            self.get_required_compiled_attr_value(&dir_attr.name, dir_attr.value, &mut dir)?;

        if let Some(extra_attr) = attrs_iter.next() {
            let e = anyhow!("extraneous attribute `{}`. Remove it", extra_attr.name);
            self.error_ctx_frag = Some(extra_attr.name);
            return Err(e);
        }

        let loop_template = take(&mut element.body);
        let mut expanded_body = Vec::new();
        for (i, entry_or_err) in read_dir(&dir)?.enumerate() {
            let entry = entry_or_err.inspect_err(|_| {
                self.error_ctx_frag = Some(dir_attr.name.clone());
            })?;
            let path = String::from_utf8(entry.path().into_os_string().into_encoded_bytes())
                .with_context(|| {
                    self.error_ctx_frag = Some(dir_attr.name.clone());
                    format!(
                        "directory `{dir}` contains a file with a non-UTF8 name: {:?}",
                        entry.path()
                    )
                })?;

            self.error_ctx_frag = Some(iter_var_name.clone());
            self.lua_ctx.set_var(&iter_var_name, &path)?;
            let mut i_str = InlineStr::<15>::default();
            write!(i_str, "{i}")?;
            self.lua_ctx.set_var("index", &i_str)?;

            let mut new_body_piece = loop_template.clone();
            for node in &mut new_body_piece {
                self.compile_node(node)?;
            }
            expanded_body.extend(new_body_piece);
        }

        element.body = expanded_body.into();
        Ok(())
    }

    fn compile_children_element(&mut self, element: &mut XmlElement) -> Result {
        self.forbid_children_in_element(element)?;

        self.error_ctx_frag = Some(take(&mut element.name));
        let Some(expansion) = self.template_expansions.last() else {
            bail!("`<$children>` outside of a template");
        };

        element.body = expansion.body.clone();
        for child in &mut element.body {
            self.compile_node(child)?;
        }
        Ok(())
    }

    fn compile_raw_element(&mut self, element: &mut XmlElement) -> Result {
        if !element.attrs.is_empty() {
            let mut attrs = take(&mut element.attrs).into_vec();
            let new_element_name_attr = attrs.remove(0);
            if let Some(value_part) = new_element_name_attr.value.parts.first() {
                self.error_ctx_frag = Some(value_part.as_text().clone());
                bail!(
                    "Expected an element name, got an attribute with a value. \
                       Remove `=` and the text after it"
                );
            }

            element.name = new_element_name_attr.name;
            element.attrs = attrs.into();
        }

        let mut escaped_body = String::new();
        for node in &mut element.body {
            self.compile_node(node)?;
            write!(escaped_body, "{node:#}")?;
        }
        element.body =
            [XmlNode::Text(XmlText { parts: [XmlTextFragment::Text(escaped_body.into())].into() })]
                .into();

        Ok(())
    }

    fn compile_register_template_ext(&mut self, element: &mut XmlElement) -> Result {
        self.forbid_children_in_element(element)?;

        let mut attrs = take(&mut element.attrs).into_vec();
        if attrs.is_empty() {
            let e = anyhow!("no file extension provided to `<{}>`", element.name);
            self.error_ctx_frag = Some(take(&mut element.name));
            return Err(e);
        }
        let ext_attr = attrs.remove(0);
        self.get_flag_value(&ext_attr)?;
        if let Some(extra) = attrs.into_iter().next() {
            self.error_ctx_frag = Some(extra.name);
            bail!("`<{}>` only accepts 1 attribute", element.name);
        }

        self.asset_manager.register_template_ext(ext_attr.name.into_os_str_view());

        element.name = "".into();
        Ok(())
    }

    fn compile_ref_like_element(&mut self, element: &mut XmlElement) -> Result {
        let is_ref_element = element.name == "$ref";

        let ref_element_name = if is_ref_element {
            self.forbid_children_in_element(element)?;
            take(&mut element.name)
        } else {
            element.name.clone()
        };
        let mut last_processing_mode_marker = None;
        let mut last_cached_marker = None;
        let expected_var_name = is_ref_element.not().then(|| match &*element.name {
            "a" | "image" | "use" | "link" => "href",
            "img" | "script" => "src",
            "form" => "action",
            _ => unreachable!("compile_ref_like_element called on element `{}`", element.name),
        });
        let mut var_name = None;
        let mut path = None;

        let mut recovered_attrs = Vec::new();
        for attr in take(&mut element.attrs) {
            match &*attr.name {
                "$raw" | "$template" => {
                    if let Some(ref marker) = last_processing_mode_marker {
                        let e =
                            anyhow!("Redundant `{}`, `{}` was already provided", attr.name, marker);
                        self.error_ctx_frag = Some(attr.name);
                        return Err(e);
                    }

                    self.get_flag_value(&attr)?;
                    last_processing_mode_marker = Some(attr.name);
                }

                "$cached" => {
                    if let Some(ref marker) = last_cached_marker {
                        self.error_ctx_frag = Some(attr.name);
                        bail!("The `{marker}` marker was already provided");
                    }

                    self.get_flag_value(&attr)?;
                    last_cached_marker = Some(attr.name);
                }

                // Ref attrs
                _ if !is_ref_element && Some(&*attr.name) == expected_var_name => {
                    self.error_ctx_frag = Some(attr.name.clone());
                    self.get_required_compiled_attr_value(
                        &attr.name,
                        attr.value.clone(),
                        &mut path,
                    )?;
                    var_name = Some(attr.name.clone());
                    recovered_attrs.push(attr);
                }

                _ if is_ref_element && attr.name.starts_with('$') => {
                    self.error_ctx_frag = Some(attr.name.clone());
                    self.get_required_compiled_attr_value(&attr.name, attr.value, &mut path)?;
                    var_name = Some(attr.name.after(1));
                }

                unknown => {
                    if is_ref_element {
                        let e = anyhow!("Unexpected attribute: {unknown}");
                        self.error_ctx_frag = Some(attr.name);
                        return Err(e);
                    }
                    recovered_attrs.push(attr);
                }
            }
        }
        element.attrs = recovered_attrs.into();

        let (Some(var_name), Some(path)) = (var_name, path) else {
            self.error_ctx_frag = Some(ref_element_name);

            bail!(
                "The asset to reference is not provided. \
                   Add an attribute in the form of `{}=\"path/to/file\"`",
                expected_var_name.unwrap_or("$VARNAME"),
            );
        };

        let is_template = last_processing_mode_marker.map(|marker| marker == "$template");
        let is_local = last_cached_marker.is_none();
        let asset =
            self.asset_manager.add_asset(path.trim_start_matches('/'), is_local, is_template)?;
        if is_ref_element {
            self.lua_ctx.set_var(&var_name, &asset.path)?;
        }

        Ok(())
    }

    fn compile_element(&mut self, element: &mut XmlElement) -> Result {
        if let "!DOCTYPE" | "area" | "base" | "br" | "col" | "embed" | "hr" | "input" | "meta"
        | "param" | "source" | "track" | "wbr" = &*element.name
        {
            self.forbid_children_in_element(element)?;
        }

        match &*element.name {
            "$template" => self.compile_template_element(element)?,
            "$foreach" => self.compile_foreach_element(element)?,
            "$children" => self.compile_children_element(element)?,
            "$raw" => self.compile_raw_element(element)?,
            "$registerTemplateExt" => self.compile_register_template_ext(element)?,
            "$ref" | "a" | "image" | "use" | "link" | "img" | "script" | "form" => {
                self.compile_ref_like_element(element)?;
            }
            template if template.starts_with('$') => self.expand_template(element)?,

            _ => {}
        }

        self.is_top_level = element.name == ROOT_ELEMEMT;
        for attr in &mut element.attrs {
            self.compile_text_node(&mut attr.value)?;
        }
        for child in &mut element.body {
            self.compile_node(child)?;
        }

        Ok(())
    }

    fn compile_node(&mut self, node: &mut XmlNode) -> Result {
        match node {
            XmlNode::Element(element) => self.compile_element(element),
            XmlNode::Text(text) => self.compile_text_node(text),
        }
    }

    fn wrap_compilation_error(&self, e: anyhow::Error) -> anyhow::Error {
        let Some(error_ctx_frag) = &self.error_ctx_frag else {
            return e;
        };
        let Some((path, loc)) = self.asset_manager.locate(error_ctx_frag.as_ptr()) else {
            return e;
        };

        e.context(ExtraCtx(FullLocation { path: path.to_string().into_path_bytes(), loc })).context(
            Expansions(
                self.template_expansions.iter().map(|x| x.template_name_in_invoc.clone()).collect(),
            ),
        )
    }

    fn rectify_html_tree(element: &mut XmlElement) {
        let mut new_body = Vec::with_capacity(element.body.len());
        for child in take(&mut element.body) {
            match child {
                XmlNode::Element(mut child_element) => {
                    Self::rectify_html_tree(&mut child_element);
                    if child_element.name.is_empty() {
                        new_body.extend(child_element.body);
                    } else {
                        new_body.push(XmlNode::Element(child_element));
                    }
                }
                text @ XmlNode::Text(_) => new_body.push(text),
            }
        }
        element.body = new_body.into();
    }

    fn postprocess_html_doc(root: &mut XmlElement) {
        Self::rectify_html_tree(root);

        let mut extra_children_in_root = Vec::with_capacity(root.body.len());
        let mut existing_html_element = None;
        let mut existing_doctype_element = None;
        for child in take(&mut root.body) {
            match &child {
                XmlNode::Element(x) if x.name == "!DOCTYPE" => {
                    existing_doctype_element = Some(child);
                }
                XmlNode::Element(x) if x.name == "html" => existing_html_element = Some(child),
                _ => extra_children_in_root.push(child),
            }
        }

        let doctype_element = existing_doctype_element.unwrap_or_else(|| {
            XmlNode::Element(XmlElement {
                name: "!DOCTYPE".into(),
                attrs: [Attr { name: "html".into(), value: default() }].into(),
                body: default(),
            })
        });
        let html_element = existing_html_element.unwrap_or_else(|| {
            XmlNode::Element(XmlElement {
                name: "html".into(),
                attrs: default(),
                body: extra_children_in_root.into(),
            })
        });
        root.body = [doctype_element, html_element].into();
        let Some(XmlNode::Element(html_element)) = root.body.last_mut() else {
            unreachable!("did not create an `<html>` element")
        };

        let mut extra_children_in_html = Vec::with_capacity(html_element.body.len());
        let mut existing_body_element = None;
        let mut existing_head_element = None;
        for child in take(&mut html_element.body) {
            match &child {
                XmlNode::Element(x) if x.name == "head" => existing_head_element = Some(child),
                XmlNode::Element(x) if x.name == "body" => existing_body_element = Some(child),
                _ => extra_children_in_html.push(child),
            }
        }

        let head_element = match existing_head_element {
            Some(XmlNode::Element(mut existing)) => {
                existing.body = chain(take(&mut existing.body), extra_children_in_html).collect();
                XmlNode::Element(existing)
            }

            _ => XmlNode::Element(XmlElement {
                name: "head".into(),
                attrs: default(),
                body: extra_children_in_html.into(),
            }),
        };
        let body_element = existing_body_element
            .unwrap_or_else(|| XmlNode::Element(XmlElement { name: "body".into(), ..default() }));
        html_element.body = [head_element, body_element].into();
    }

    pub fn compile(&mut self) -> Result {
        while let Some((src, asset)) = self.asset_manager.next_uncompiled_asset() {
            let mut ast_builder = AstBuilder::new(src, &asset);
            let mut root = XmlElement {
                name: ROOT_ELEMEMT.into(),
                attrs: default(),
                body: ast_builder.by_ref().collect(),
            };

            ast_builder.finish()?;
            self.compile_element(&mut root).map_err(|e| self.wrap_compilation_error(e))?;
            root.name = "".into();

            if &*asset.ext == "html" {
                Self::postprocess_html_doc(&mut root);
            }

            self.asset_manager.save_compilation_result(XmlNode::Element(root));
        }
        Ok(())
    }
}

pub fn compile(
    src: impl AsRef<str>,
    dst_root: impl AsRef<Path> + Send,
) -> impl Future<Output = Result> + Send {
    // Separated to drop non-Send objects (LuaCtx et al.) before starting an async ctx
    let asset_manager = (|| {
        let mut compiler = Compiler::new();

        compiler.asset_manager.add_asset(StrView::from_str(src.as_ref()), true, None)?;
        compiler
            .compile()
            .map_err(|e| collect_template_expansion_info(&e, compiler.asset_manager.assets()))?;
        anyhow::Ok(compiler.asset_manager)
    })();

    async move {
        let mut asset_manager = asset_manager?;
        asset_manager.write_to_disk(dst_root).await
    }
}
