use {
    crate::{
        asset::{Asset, AssetCategory, AssetManager},
        bail, ensure,
        error::{AnyhowResultExt, Expansions, ExtraCtx, collect_template_expansion_info},
        lexer::{TextLexeme, TextLexer, url_scheme},
        parser::{Attr, Element, HtmlParser, MarkdownParser, Node, Text},
        utils::{
            HomogenousResultExt, InlineStr, OptionExt, Result, ResultExt, default,
            rel_link_to_file_path,
        },
        view::StrView,
    },
    anyhow::Context,
    mlua::{FromLua, Lua, Value},
    shrimple_parser::Input,
    std::{
        cell::Cell,
        cmp::min,
        fmt::Write,
        fs::read_dir,
        iter::chain,
        mem::{replace, take},
        path::Path,
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
    fn with_var<R>(&self, name: &StrView, f: impl FnOnce(&str) -> R) -> Result<R> {
        let res = (|| -> Result<R> {
            let str = self.inner.globals().raw_get::<&str, Option<mlua::String>>(name)?;
            Ok(match str {
                Some(s) => f(s.to_str()?),
                None => f(""),
            })
        })();
        res.with_span(name)
    }

    fn set_var(&self, name: &StrView, value: &str) -> Result {
        self.inner.globals().set(&**name, value).adapt_err().with_span(name)
    }

    fn with_eval_result<R>(&self, code: &StrView, f: impl FnOnce(&str) -> R) -> Result<R> {
        let res = (|| -> Result<R> {
            Ok(match self.inner.load(&**code).set_name("").eval()? {
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
        })();
        res.with_span(code)
    }
}

struct Param {
    name: StrView,
    default: Option<StrView>,
}

impl Param {
    const fn is_required(&self) -> bool {
        self.default.is_none()
    }
}

struct Template {
    name: StrView,
    accepts_children: bool,
    /// Sorted by name
    params: Box<[Param]>,
    body: Box<[Node]>,
}

impl Template {
    fn get_param(&self, name: &StrView) -> Result<&Param> {
        let Ok(param_id) = self.params.binary_search_by_key(&name, |param| &param.name) else {
            bail!(span: name.clone(), "template `{}` has no parameter named `{}`", self.name, name);
        };
        Ok(&self.params[param_id])
    }
}

struct TemplateExpansion {
    template_name_in_invoc: StrView,
    body: Box<[Node]>,
}

#[derive(Default)]
struct Templates(Vec<Template>);

impl Templates {
    fn add(&mut self, template: Template) -> Result {
        let Err(i) = self.0.binary_search_by_key(&&template.name, |t| &t.name) else {
            bail!(span: template.name, "template `{}` is already defined", template.name);
        };
        self.0.insert(i, template);
        Ok(())
    }

    fn get(&self, name: &StrView) -> Result<&Template> {
        let Ok(template_id) = self.0.binary_search_by_key(&name, |t| &t.name) else {
            bail!(span: name.clone(), "template `{name}` is not defined");
        };
        Ok(&self.0[template_id])
    }
}

struct Compiler {
    is_top_level: bool,
    asset_manager: AssetManager,
    lua_ctx: LuaCtx,
    templates: Templates,
    template_expansions: Vec<TemplateExpansion>,
}

fn forbid_children_in_element(element: &Element) -> Result {
    ensure!(
        span: element.name.clone(),
        element.body.is_empty(),
        "`{}` element cannot have children",
        element.name,
    );
    Ok(())
}

impl Compiler {
    // TODO: make customisable
    const SPACES_PER_INDENT: usize = 4;

    fn new() -> Self {
        Self {
            is_top_level: false,
            asset_manager: default(),
            lua_ctx: default(),
            templates: default(),
            template_expansions: default(),
        }
    }

    fn compile_text_lexeme(&self, fragment: &TextLexeme, dst: &mut String) -> Result<()> {
        match fragment {
            TextLexeme::Var(name) => {
                self.lua_ctx
                    .with_var(name, |v| dst.write_str(v))
                    .adapt_err()
                    .flatten()
                    .with_span(name)?;
            }
            TextLexeme::Expr(code) => {
                self.lua_ctx
                    .with_eval_result(code, |res| dst.write_str(res))
                    .adapt_err()
                    .flatten()
                    .with_span(code)?;
            }
            TextLexeme::Text(text) => dst.write_str(text)?,
        }

        Ok(())
    }

    fn compile_text_node(&self, node: &mut Text) -> Result {
        if node.parts.is_empty() {
            return Ok(());
        }

        let mut res = String::new();
        for part in take(&mut node.parts) {
            self.compile_text_lexeme(&part, &mut res)?;
        }
        node.parts = [TextLexeme::Text(res.into())].into();
        Ok(())
    }

    fn get_required_compiled_attr_value<'dst>(
        &self,
        name: &StrView,
        mut src: Text,
        dst: &'dst mut Option<StrView>,
    ) -> Result<&'dst mut StrView> {
        ensure!(span: name.clone(), dst.is_none(), "`{name}` attribute is already provided");

        self.compile_text_node(&mut src)?;
        Ok(dst.insert(
            src.parts
                .into_iter()
                .next()
                .with_context(|| format!("`{name}` attribute must have a value"))
                .with_span(name)?
                .into_text(),
        ))
    }

    fn get_optional_compiled_attr_value(
        &self,
        name: &StrView,
        mut src: Text,
        dst: &mut Option<StrView>,
    ) -> Result {
        ensure!(span: name.clone(), dst.is_none(), "`{name}` attribute is already provided");

        self.compile_text_node(&mut src)?;
        *dst = src.parts.into_iter().next().map(TextLexeme::into_text);
        Ok(())
    }

    // TODO: come up with a way to control the value of boolean flags via Lua.
    // Which string values do we treat as `true`? which as `false`? should any other values cause
    // an error?
    #[expect(clippy::unused_self, reason = "later this fn will eval lua")]
    fn get_flag_value(&self, attr: &Attr, name_span: &mut Option<StrView>) -> Result<bool> {
        if let Some(existing) = name_span {
            if &attr.name == existing {
                bail!(span: attr.name.clone(), "`{existing}` attribute is already provided");
            }
            bail!(span: attr.name.clone(), "Unnecessary `{}`, `{existing}` alreay provided", attr.name);
        }
        *name_span = Some(attr.name.clone());

        match &*attr.value.parts {
            [] => Ok(true),
            _ => bail!(
                span: attr.name.clone(),
                "`{}` attribute must not have a value",
                attr.name,
            ),
        }
    }

    fn compile_template_element(&mut self, element: &mut Element) -> Result {
        let element_name = take(&mut element.name);
        ensure!(
            span: element_name,
            self.is_top_level,
            "`<{}>` is only allowed at the top level",
            element.name,
        );

        let mut name = None;
        let mut accepts_children = false;
        let mut accepts_children_name_span = None;
        let mut params = vec![];

        for attr in take(&mut element.attrs) {
            match &*attr.name {
                "name" => {
                    self.get_required_compiled_attr_value(&attr.name, attr.value, &mut name)?;
                    ensure!(
                        span: attr.name,
                        !name.as_deref().is_none_or(str::is_empty),
                        "template name cannot be empty",
                    );
                }
                "acceptsChildren" => {
                    accepts_children =
                        self.get_flag_value(&attr, &mut accepts_children_name_span)?;
                }
                param if param.starts_with('$') => {
                    let mut default = None;
                    self.get_optional_compiled_attr_value(&attr.name, attr.value, &mut default)?;
                    let name = attr.name.after(1);
                    params.push(Param { name, default });
                }
                unknown => bail!(
                    span: attr.name,
                    "`$template` has no attribute named `{unknown}`",
                ),
            }
        }

        let Some(name) = name else {
            bail!(span: take(&mut element.name), "template name must be provided");
        };

        self.templates.add(Template {
            name,
            accepts_children,
            params: params.into(),
            body: take(&mut element.body),
        })?;
        Ok(())
    }

    // TODO: consider expanding `<$children />` to an ad-hoc template to reduce copying
    fn expand_template(&mut self, element: &mut Element) -> Result {
        let name = take(&mut element.name).after(1);
        let template = self.templates.get(&name)?;

        if !template.accepts_children {
            forbid_children_in_element(element)?;
        }
        self.template_expansions.push(TemplateExpansion {
            body: replace(&mut element.body, template.body.clone()),
            template_name_in_invoc: name,
        });

        for param in &template.params {
            if element.attrs.iter().any(|arg| arg.name == param.name) {
                continue;
            }

            let Some(default) = param.default.as_ref() else {
                bail!(span: param.name.clone(), "no value provided for parameter `{}`", param.name);
            };
            self.lua_ctx.set_var(&param.name, default)?;
        }

        for arg in take(&mut element.attrs) {
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

    fn compile_foreach_element(&mut self, element: &mut Element) -> Result {
        let mut attrs_iter = take(&mut element.attrs).into_iter();

        let Some(iter_var_name_attr) = attrs_iter.next() else {
            bail!(
                span: take(&mut element.name),
                "expected the iterator variable declaration, got nothing",
            );
        };
        if !iter_var_name_attr.name.starts_with('$') {
            bail!(
                span: iter_var_name_attr.name,
                "expected the iterator variable declaration. Prefix the variable name with `$`",
            );
        }
        if let Some(value_part) = iter_var_name_attr.value.parts.first() {
            bail!(
                span: value_part.as_text().clone(),
                "the iterator variable cannot have a pre-defined value. \
                 Remove `=` and the value after it",
            );
        }
        let iter_var_name = iter_var_name_attr.name.after(1);

        let Some(in_kw) = attrs_iter.next() else {
            bail!(span: iter_var_name.end(), "expected `in`, got nothing");
        };

        ensure!(
            span: in_kw.name,
            in_kw.name == "in",
            "expected `in`, got `{}`",
            in_kw.name,
        );
        self.get_flag_value(&in_kw, &mut None)?;

        if let Some(value_part) = in_kw.value.parts.first() {
            bail!(
                span: value_part.as_text().clone(),
                "extraneous text starting from `=`. Remove it",
            );
        }

        let Some(dir_attr) = attrs_iter.next() else {
            bail!(span: in_kw.name.end(), "expected the `dir` iterator, found nothing");
        };
        ensure!(span: dir_attr.name, dir_attr.name == "dir", "expected `dir`, got `{}`", dir_attr.name);
        let mut dir = None;
        let dir =
            self.get_required_compiled_attr_value(&dir_attr.name, dir_attr.value, &mut dir)?;

        if let Some(extra_attr) = attrs_iter.next() {
            bail!(
                span: extra_attr.name.start(),
                "extraneous attributes. Remove them",
            );
        }

        // TODO: customisable order
        let mut file_paths = Vec::new();
        for entry_or_err in read_dir(&dir)
            .with_context(|| format!("failed to start reading the directory `{dir}`"))
            .with_span(&dir_attr.name)?
        {
            let entry = entry_or_err
                .with_context(|| format!("failed to fetch an entry in the directory `{dir}`"))
                .with_span(&dir_attr.name)?;
            let path = String::from_utf8(entry.path().into_os_string().into_encoded_bytes())
                .with_context(|| {
                    format!(
                        "directory `{dir}` contains a file with a non-UTF8 name: {:?}",
                        entry.path(),
                    )
                })?
                .into_boxed_str();

            let new_file_path_id = file_paths.binary_search(&path).merge();
            file_paths.insert(new_file_path_id, path);
        }

        let loop_template = take(&mut element.body);
        let mut expanded_body = Vec::new();
        for (i, path) in file_paths.iter().enumerate() {
            self.lua_ctx.set_var(&iter_var_name, path)?;
            let mut i_str = InlineStr::<15>::default();
            write!(i_str, "{i}")?;
            self.lua_ctx.set_var(&StrView::from("index"), &i_str)?;

            let mut new_body_piece = loop_template.clone();
            for node in &mut new_body_piece {
                self.compile_node(node)?;
            }
            expanded_body.extend(new_body_piece);
        }

        element.body = expanded_body.into();
        Ok(())
    }

    fn compile_children_element(&mut self, element: &mut Element) -> Result {
        forbid_children_in_element(element)?;

        let element_name = take(&mut element.name);
        let Some(expansion) = self.template_expansions.last() else {
            bail!(span: element_name, "`<{}>` outside of a template", element.name);
        };

        element.body = expansion.body.clone();
        for child in &mut element.body {
            self.compile_node(child)?;
        }
        Ok(())
    }

    fn compile_raw_element(&mut self, element: &mut Element) -> Result {
        if !element.attrs.is_empty() {
            let mut attrs = take(&mut element.attrs).into_vec();
            let new_element_name_attr = attrs.remove(0);
            self.get_flag_value(&new_element_name_attr, &mut None)?;

            element.name = new_element_name_attr.name;
            element.attrs = attrs.into();
        }

        let mut escaped_body = String::new();
        for node in &mut element.body {
            self.compile_node(node)?;
            write!(escaped_body, "{node:#}")?;
        }
        element.body = [StrView::from(escaped_body).into()].into();

        Ok(())
    }

    fn compile_register_asset_ext_element(&mut self, element: &mut Element) -> Result {
        forbid_children_in_element(element)?;

        let element_name = take(&mut element.name);
        ensure!(
            span: element_name,
            !element.attrs.is_empty(),
            "`<{}>` must have at least 1 attribute",
            element.name,
        );

        for decl in take(&mut element.attrs) {
            let mut category_name_buf = None;
            let decl_value_start = decl.value.parts.first().map(|l| l.as_text().clone().start());
            let category = self
                .get_required_compiled_attr_value(&decl.name, decl.value, &mut category_name_buf)?
                .parse::<AssetCategory>()
                .maybe_with_span(decl_value_start.as_ref())?;
            self.asset_manager.register_asset_ext(decl.name.into_os_str_view(), category);
        }
        Ok(())
    }

    fn compile_ref_element(&mut self, element: &mut Element, ref_attr_name: &str) -> Result {
        let mut last_asset_category_marker = None;
        let mut last_cached_marker = None;
        let mut cached = false;
        let mut var_name = None;
        let mut path = None;
        let mut wrapping_template_name = None;
        let mut wrap_in_attr_name = StrView::default();
        let mut ref_attr_name_span = None;

        let mut recovered_attrs = Vec::new();
        for attr in take(&mut element.attrs) {
            match &*attr.name {
                "$cached" => {
                    cached = self.get_flag_value(&attr, &mut last_cached_marker)?;
                }

                "$var" => {
                    self.get_required_compiled_attr_value(
                        &attr.name,
                        attr.value.clone(),
                        &mut var_name,
                    )?;
                }

                "$wrapIn" => {
                    self.get_required_compiled_attr_value(
                        &attr.name,
                        attr.value,
                        &mut wrapping_template_name,
                    )?;
                    wrap_in_attr_name = attr.name;
                }

                _ if attr.name.starts_with('$') => {
                    self.get_flag_value(&attr, &mut last_asset_category_marker)?;
                }

                _ => {
                    if &*attr.name == ref_attr_name {
                        ref_attr_name_span = Some(attr.name.clone());
                        self.get_required_compiled_attr_value(
                            &attr.name,
                            attr.value.clone(),
                            &mut path,
                        )?;
                    }
                    recovered_attrs.push(attr);
                }
            }
        }
        element.attrs = recovered_attrs.into();

        let (Some(path), Some(ref_attr_name_span)) = (path, ref_attr_name_span) else {
            bail!(span: take(&mut element.name), "Missing `{}` attribute", ref_attr_name);
        };

        if !cached && url_scheme(&path).is_some() {
            return Ok(());
        }
        let asset_category = last_asset_category_marker
            .as_deref()
            .try_map(str::parse)
            .maybe_with_span(last_asset_category_marker.as_ref())?;

        let normalised: StrView = rel_link_to_file_path(&path).into_owned().into();
        let asset = self
            .asset_manager
            .add_asset(normalised.clone(), !cached, asset_category)
            .with_span(&ref_attr_name_span)?;

        if let Some(wrapping_element_name) = wrapping_template_name {
            let template = self.templates.get(&wrapping_element_name)?;
            ensure!(
                span: wrap_in_attr_name,
                template.accepts_children,
                "Template `{}` doesn't accept children, thus it can't wrap an asset",
                template.name,
            );

            for param in &template.params {
                ensure!(
                    span: wrap_in_attr_name,
                    !param.is_required(),
                    "Template `{}` has required parameters (e.g. `{}`), thus it can't wrap an asset",
                     template.name,
                     param.name,
                );
            }

            if !asset.set_wrapping_template_name(wrapping_element_name) {
                // TODO: make this a warning
                bail!(
                    span: wrap_in_attr_name,
                    "The declared asset is raw, i.e. it won't be compiled, \
                    and thus it can't accept the `{wrap_in_attr_name}` attribute",
                );
            }
        }

        if cached {
            let Some(ref_attr) = element.attr_mut(ref_attr_name) else {
                bail!(
                    span: element.name.clone(),
                    "bug: ref attr `{ref_attr_name}` disappeared from `<{}>`", element.name);
            };
            ref_attr.value = asset.path.clone().into();
        }

        if let Some(var_name) = var_name {
            self.lua_ctx.set_var(&var_name, &normalised)?;
        }

        Ok(())
    }

    fn compile_element(&mut self, element: &mut Element) -> Result {
        if let "!DOCTYPE" | "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input"
        | "link" | "meta" | "param" | "source" | "track" | "wbr" = &*element.name
        {
            forbid_children_in_element(element)?;
        }

        match &*element.name {
            "$template" => self.compile_template_element(element)?,
            "$foreach" => self.compile_foreach_element(element)?,
            "$children" => self.compile_children_element(element)?,
            "$raw" => self.compile_raw_element(element)?,
            "$registerAssetExt" => self.compile_register_asset_ext_element(element)?,
            "a" | "image" | "use" | "link" => self.compile_ref_element(element, "href")?,
            "img" | "script" => self.compile_ref_element(element, "src")?,
            "form" => self.compile_ref_element(element, "action")?,

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

    fn compile_node(&mut self, node: &mut Node) -> Result {
        match node {
            Node::Element(element) => self.compile_element(element),
            Node::Text(text) => self.compile_text_node(text),
        }
    }

    fn wrap_compilation_error(&self, e: anyhow::Error) -> anyhow::Error {
        let Some(span) = e.downcast_ref::<ExtraCtx<StrView>>() else {
            return e;
        };

        let Some((path, loc)) = self.asset_manager.locate(span.0.as_ptr()) else {
            return e;
        };

        e.context(ExtraCtx(loc.with_path(path.to_string()))).context(Expansions(
            self.template_expansions.iter().map(|x| x.template_name_in_invoc.clone()).collect(),
        ))
    }

    fn rectify_html_tree(element: &mut Element) {
        let mut new_body = Vec::with_capacity(element.body.len());
        for child in take(&mut element.body) {
            match child {
                Node::Element(mut child_element) => {
                    Self::rectify_html_tree(&mut child_element);
                    if child_element.name.is_empty() {
                        new_body.extend(child_element.body);
                    } else {
                        new_body.push(Node::Element(child_element));
                    }
                }
                text @ Node::Text(_) => new_body.push(text),
            }
        }
        element.body = new_body.into();
    }

    fn compile_markdown(&mut self, node: &mut Node) -> Result {
        match node {
            Node::Element(element) => {
                for child in &mut element.body {
                    self.compile_markdown(child)?;
                }
            }
            Node::Text(text) => {
                let Some(first_part) = text.parts.first() else {
                    return Ok(());
                };

                let mut lines = (**first_part.as_text()).lines();
                let Some(mut without_indentation) = lines.next().map(str::to_owned) else {
                    return Ok(());
                };
                let expected_indent_len = text.indent_level * Self::SPACES_PER_INDENT;
                for line in lines {
                    let indent_len = line.bytes().take_while(|&b| b == b' ').count();
                    without_indentation.push('\n');
                    without_indentation.push_str(&line[min(indent_len, expected_indent_len)..]);
                }

                let nodes = MarkdownParser::new(&without_indentation).collect::<Vec<_>>();
                *node = nodes.into();
                self.compile_node(node)?;
            }
        }

        Ok(())
    }

    fn postprocess_html_doc(&mut self, root: &mut Element) -> Result {
        for child in &mut root.body {
            self.compile_markdown(child)?;
        }

        Self::rectify_html_tree(root);

        let mut extra_children_in_root = Vec::with_capacity(root.body.len());
        let mut existing_html_element = None;
        let mut existing_doctype_element = None;
        for child in take(&mut root.body) {
            match &child {
                Node::Element(x) if x.name == "!DOCTYPE" => {
                    existing_doctype_element = Some(child);
                }
                Node::Element(x) if x.name == "html" => existing_html_element = Some(child),
                _ => extra_children_in_root.push(child),
            }
        }

        let doctype_element = existing_doctype_element.unwrap_or_else(|| {
            Node::Element(Element {
                name: "!DOCTYPE".into(),
                attrs: [("html", None).into()].into(),
                body: default(),
            })
        });
        let html_element = existing_html_element.unwrap_or_else(|| {
            Node::Element(Element {
                name: "html".into(),
                attrs: default(),
                body: extra_children_in_root.into(),
            })
        });
        root.body = [doctype_element, html_element].into();
        let Some(Node::Element(html_element)) = root.body.last_mut() else {
            unreachable!("did not create an `<html>` element")
        };

        let mut extra_children_in_html = Vec::new();
        let mut head_elements_in_html = Vec::new();
        let mut existing_body_element = None;
        let mut existing_head_element = None;
        for child in take(&mut html_element.body) {
            match child {
                Node::Element(x) if x.name == "head" => existing_head_element = Some(x),
                Node::Element(x) if x.name == "body" => existing_body_element = Some(x),
                Node::Element(ref x)
                    if let "title" | "base" | "link" | "style" | "meta" | "script" | "noscript"
                    | "template" = &*x.name =>
                {
                    head_elements_in_html.push(child);
                }
                extra => extra_children_in_html.push(extra),
            }
        }

        let mut head_element = match existing_head_element {
            Some(mut existing) => {
                existing.body = chain(take(&mut existing.body), head_elements_in_html).collect();
                existing
            }

            None => Element {
                name: "head".into(),
                attrs: default(),
                body: head_elements_in_html.into(),
            },
        };

        if head_element.subelements("meta").all(|meta| meta.attr("charset").is_none()) {
            head_element.body = chain(
                [Node::Element(Element {
                    name: "meta".into(),
                    attrs: [("charset", Some("UTF-8")).into()].into(),
                    body: default(),
                })],
                take(&mut head_element.body),
            )
            .collect();
        }

        let body_element = existing_body_element.unwrap_or_else(|| Element {
            name: "body".into(),
            attrs: default(),
            body: take(&mut extra_children_in_html).into(),
        });
        html_element.body =
            chain(extra_children_in_html, [head_element, body_element].map(Node::Element))
                .collect();

        Ok(())
    }

    fn parse_template(src: StrView, asset: Asset) -> Result<Node> {
        let result = Cell::new(Ok(()));
        let node = Node::Text(Text {
            indent_level: 0,
            parts: TextLexer::new(src, asset, &result).collect(),
        });
        result.into_inner().map(|_| node)
    }

    fn parse_document(src: StrView, asset: &Asset) -> Result<Node> {
        let result = Cell::new(Ok(()));
        let root = Node::Element(Element {
            name: ROOT_ELEMEMT.into(),
            attrs: default(),
            body: HtmlParser::new(src, asset, &result).collect(),
        });
        result.into_inner().map(|_| root)
    }

    fn compile(&mut self) -> Result {
        while let Some((input, asset)) = self.asset_manager.next_uncompiled_asset() {
            let category = asset.category;
            let mut node = match category {
                AssetCategory::Raw => continue,
                AssetCategory::Template => Self::parse_template(input.src, asset)?,
                AssetCategory::HtmlDocument | AssetCategory::Document => {
                    Self::parse_document(input.src, &asset)?
                }
            };

            if let Some(wrapping_template_name) = input.wrapping_template_name {
                node = Node::Element(Element {
                    name: format!("${wrapping_template_name}").into(),
                    attrs: default(),
                    body: [node].into(),
                });
            }

            self.compile_node(&mut node).map_err(|e| self.wrap_compilation_error(e))?;

            if let Node::Element(root) = &mut node {
                root.name = "".into();
                if category == AssetCategory::HtmlDocument {
                    self.postprocess_html_doc(root).map_err(|e| self.wrap_compilation_error(e))?;
                }
            }

            self.asset_manager.save_compilation_result(node);
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
