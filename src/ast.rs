//! Abstract Syntax Tree of an XML document

use {
    crate::{
        asset::Asset,
        error::ExtraCtx,
        escape_html::EscapeHtml,
        parser::{Parser, TextParser, XmlFragment, XmlTextFragment},
        utils::{Peekable, Result, default},
        view::StrView,
    },
    anyhow::anyhow,
    shrimple_parser::utils::{FullLocation, PathLike, locate},
    std::fmt::{self, Display, Formatter},
};

#[derive(Debug, Clone)]
pub struct Attr {
    pub name: StrView,
    pub value: XmlText,
}

impl From<(&'static str, Option<&'static str>)> for Attr {
    fn from((name, value): (&'static str, Option<&'static str>)) -> Self {
        Self { name: name.into(), value: value.map_or_else(default, Into::into) }
    }
}

impl Display for Attr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(" ")?;
        EscapeHtml(&self.name).fmt(f)?;
        if !self.value.parts.is_empty() {
            f.write_str("=\"")?;
            self.value.fmt(f)?;
            f.write_str("\"")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct XmlElement {
    /// If empty, only the element's children are printed
    pub name: StrView,
    pub attrs: Box<[Attr]>,
    pub body: Box<[XmlNode]>,
}

impl Display for XmlElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if !self.name.is_empty() {
            EscapeHtml("<").fmt(f)?;
            EscapeHtml(&self.name).fmt(f)?;
            self.attrs.iter().try_for_each(|attr| attr.fmt(f))?;
            match &*self.name {
                "!DOCTYPE" | "area" | "base" | "br" | "col" | "embed" | "hr" | "input" | "meta"
                | "param" | "source" | "track" | "wbr" => return EscapeHtml("/>").fmt(f),
                _ => (),
            }
            EscapeHtml(">").fmt(f)?;
        }

        self.body.iter().try_for_each(|child| child.fmt(f))?;

        if !self.name.is_empty() {
            EscapeHtml("</").fmt(f)?;
            EscapeHtml(&self.name).fmt(f)?;
            EscapeHtml(">").fmt(f)?;
        }
        Ok(())
    }
}

impl XmlElement {
    pub fn attr<'a>(&'a self, name: &str) -> Option<&'a Attr> {
        self.attrs.iter().find(|attr| attr.name == name)
    }

    pub fn attr_mut<'a>(&'a mut self, name: &str) -> Option<&'a mut Attr> {
        self.attrs.iter_mut().find(|attr| attr.name == name)
    }

    pub fn subelements<'a>(&'a self, name: &str) -> impl Iterator<Item = &'a Self> {
        self.body.iter().filter_map(move |x| x.as_element(name))
    }

    pub fn subelements_mut<'a>(&'a mut self, name: &str) -> impl Iterator<Item = &'a mut Self> {
        self.body.iter_mut().filter_map(move |x| x.as_element_mut(name))
    }

    /// Must be at the state <elementName ...
    ///                                   ^
    fn parse(
        name: StrView,
        iter: &mut Peekable<impl Iterator<Item = XmlFragment>>,
        asset: &Asset,
    ) -> Result<Self> {
        let mut attrs = Vec::new();
        let mut children = Vec::new();

        let opening_tag_end = loop {
            match iter.next() {
                None => {
                    return Err(anyhow!("unclosed opening tag")
                        .context(ExtraCtx(XmlFragment::OpeningTagStart(name))));
                }
                Some(XmlFragment::Attr(attr)) => {
                    let (name, value) = attr.into_parts();
                    attrs.push(Attr {
                        name,
                        value: XmlText {
                            parts: match value {
                                None => default(),
                                Some(
                                    code @ (XmlTextFragment::Var(_) | XmlTextFragment::Expr(_)),
                                ) => [code].into(),
                                Some(XmlTextFragment::Text(text)) => {
                                    let mut parser = TextParser::new(text, asset.clone());
                                    let parts = parser.by_ref().collect();
                                    parser.finish()?;
                                    parts
                                }
                            },
                        },
                    });
                }
                Some(XmlFragment::OpeningTagEnd(opening_tag_end)) => break opening_tag_end,
                frag => return Err(anyhow!("unexpected input").context(ExtraCtx(frag))),
            }
        };

        if !opening_tag_end.is_self_closing() {
            loop {
                if let Some(closing_name) = iter.next_if_map(|frag| match frag {
                    XmlFragment::ClosingTag(closing_name) => Ok(closing_name),
                    frag => Err(frag),
                }) {
                    if !closing_name.is_empty() && closing_name != name {
                        return Err(anyhow!("expected `</{name}>` or `</>`")
                            .context(ExtraCtx(XmlFragment::ClosingTag(closing_name))));
                    }
                    break;
                } else if iter.peek().is_none() {
                    return Err(anyhow!("unclosed element")
                        .context(ExtraCtx(XmlFragment::OpeningTagStart(name))));
                }

                children.push(XmlNode::parse(iter, asset)?);
            }
        }

        Ok(Self { name, attrs: attrs.into_boxed_slice(), body: children.into_boxed_slice() })
    }
}

#[derive(Debug, Clone, Default)]
pub struct XmlText {
    pub parts: Box<[XmlTextFragment]>,
}

impl From<StrView> for XmlText {
    fn from(value: StrView) -> Self {
        Self { parts: [XmlTextFragment::Text(value)].into() }
    }
}

impl From<&'static str> for XmlText {
    fn from(value: &'static str) -> Self {
        Self { parts: [XmlTextFragment::Text(value.into())].into() }
    }
}

impl Display for XmlText {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.parts.iter().try_for_each(|part| part.fmt(f))
    }
}

impl XmlText {
    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    fn parse(iter: &mut Peekable<impl Iterator<Item = XmlFragment>>) -> Self {
        let mut parts = Vec::new();

        while let Some(frag) = iter.next_if_map(|frag| match frag {
            XmlFragment::Text(text_frag) => Ok(text_frag),
            other => Err(other),
        }) {
            parts.push(frag);
        }

        Self { parts: parts.into_boxed_slice() }
    }
}

#[derive(Debug, Clone)]
pub enum XmlNode {
    Element(XmlElement),
    Text(XmlText),
}

impl Default for XmlNode {
    fn default() -> Self {
        Self::Text(default())
    }
}

impl Display for XmlNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Element(element) => element.fmt(f),
            Self::Text(text) => text.fmt(f),
        }
    }
}

impl XmlNode {
    pub fn as_element<'a>(&'a self, name: &str) -> Option<&'a XmlElement> {
        match self {
            Self::Element(element) => (element.name == name).then_some(element),
            Self::Text(_) => None,
        }
    }

    pub fn as_element_mut<'a>(&'a mut self, name: &str) -> Option<&'a mut XmlElement> {
        match self {
            Self::Element(element) => (element.name == name).then_some(element),
            Self::Text(_) => None,
        }
    }

    fn parse(
        iter: &mut Peekable<impl Iterator<Item = XmlFragment>>,
        asset: &Asset,
    ) -> Result<Self> {
        if let Some(name) = iter.next_if_map(|frag| match frag {
            XmlFragment::OpeningTagStart(name) => Ok(name),
            frag => Err(frag),
        }) {
            XmlElement::parse(name, iter, asset).map(Self::Element)
        } else {
            Ok(Self::Text(XmlText::parse(iter)))
        }
    }
}

pub struct AstBuilder<'asset> {
    // TODO: make our own peekable with an accessible inner iter
    iter: Peekable<Parser>,
    asset: &'asset Asset,
    error: Result,
}

impl Iterator for AstBuilder<'_> {
    type Item = XmlNode;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.peek()?;
        XmlNode::parse(&mut self.iter, self.asset)
            .map_err(|e| {
                self.error = 'e: {
                    let Some(ExtraCtx(frag)) = e.downcast_ref::<ExtraCtx<XmlFragment>>() else {
                        break 'e Err(e);
                    };
                    let Some(src) = self.asset.template_src() else {
                        break 'e Err(e);
                    };
                    let Some(loc) = locate(frag.as_src_ptr(), src) else {
                        break 'e Err(e);
                    };
                    Err(e.context(ExtraCtx(FullLocation {
                        path: self.asset.path.to_string().into_path_bytes(),
                        loc,
                    })))
                }
            })
            .ok()
    }
}

impl<'asset> AstBuilder<'asset> {
    pub fn new(src: StrView, asset: &'asset Asset) -> Self {
        let parser = Parser::new(src, asset.clone());
        Self { iter: Peekable::new(parser), asset, error: Ok(()) }
    }

    pub fn finish(self) -> Result {
        self.iter.into_inner().finish()?;
        self.error?;
        Ok(())
    }
}
