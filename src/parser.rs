//! Abstract Syntax Tree of an XML document

use {
    crate::{
        asset::Asset,
        error::ExtraCtx,
        escape_html::EscapeHtml,
        lexer::{Lexeme, Lexer, TextLexeme, TextLexer},
        utils::{Result, default},
        view::StrView,
    },
    anyhow::anyhow,
    pulldown_cmark::{Event, HeadingLevel, Options, Tag},
    shrimple_parser::utils::{FullLocation, PathLike, locate},
    std::{
        cell::Cell,
        fmt::{self, Display, Formatter},
        iter::Peekable,
        mem::take,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attr {
    pub name: StrView,
    pub value: Text,
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

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Element {
    /// If empty, only the element's children are printed
    pub name: StrView,
    pub attrs: Box<[Attr]>,
    pub body: Box<[Node]>,
}

impl Display for Element {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if !self.name.is_empty() {
            write!(f, "<{:#}", EscapeHtml(&self.name))?;
            self.attrs.iter().try_for_each(|attr| attr.fmt(f))?;
            match &*self.name {
                "!DOCTYPE" | "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input"
                | "link" | "meta" | "param" | "source" | "track" | "wbr" => {
                    return write!(f, "/>");
                }
                _ => (),
            }
            write!(f, ">")?;
        }

        self.body.iter().try_for_each(|child| child.fmt(f))?;

        if !self.name.is_empty() {
            write!(f, "</{:#}>", EscapeHtml(&self.name))?;
        }
        Ok(())
    }
}

impl Element {
    /// Must be at the state <elementName ...
    ///                                   ^
    fn parse(
        name: StrView,
        iter: &mut Peekable<impl Iterator<Item = Lexeme>>,
        indent_level: usize,
        asset: &Asset,
    ) -> Result<Self> {
        let mut attrs = Vec::new();
        let mut children = Vec::new();

        let opening_tag_end = loop {
            match iter.next() {
                None => {
                    return Err(anyhow!("unclosed opening tag")
                        .context(ExtraCtx(Lexeme::OpeningTagStart(name))));
                }
                Some(Lexeme::Attr(attr)) => {
                    let (name, value) = attr.into_parts();
                    attrs.push(Attr {
                        name,
                        value: Text {
                            indent_level: 0,
                            parts: match value {
                                None => default(),
                                Some(code @ (TextLexeme::Var(_) | TextLexeme::Expr(_))) => {
                                    [code].into()
                                }
                                Some(TextLexeme::Text(text)) => {
                                    let result = Cell::new(Ok(()));
                                    let mut parser = TextLexer::new(text, asset.clone(), &result);
                                    let parts = parser.by_ref().collect();
                                    result.into_inner()?;
                                    parts
                                }
                            },
                        },
                    });
                }
                Some(Lexeme::OpeningTagEnd(opening_tag_end)) => break opening_tag_end,
                frag => return Err(anyhow!("unexpected input").context(ExtraCtx(frag))),
            }
        };

        if !opening_tag_end.is_self_closing() {
            loop {
                if let Some(closing_name) = iter.next_if_map(|frag| match frag {
                    Lexeme::ClosingTag(closing_name) => Ok(closing_name),
                    frag => Err(frag),
                }) {
                    if !closing_name.is_empty() && closing_name != name {
                        return Err(anyhow!("expected `</{name}>` or `</>`")
                            .context(ExtraCtx(Lexeme::ClosingTag(closing_name))));
                    }
                    break;
                } else if iter.peek().is_none() {
                    return Err(anyhow!("unclosed element")
                        .context(ExtraCtx(Lexeme::OpeningTagStart(name))));
                }

                children.push(Node::parse(iter, indent_level + 1, asset)?);
            }
        }

        Ok(Self { name, attrs: attrs.into_boxed_slice(), body: children.into_boxed_slice() })
    }

    fn parse_markdown<'a>(
        iter: &mut Peekable<impl Iterator<Item = Event<'a>>>,
        tag: Tag<'a>,
    ) -> Self {
        let end_event = Event::End(tag.to_end());
        let mut attrs = Vec::new();
        let name = match tag {
            Tag::Heading { level, .. } => match level {
                HeadingLevel::H1 => "h1",
                HeadingLevel::H2 => "h2",
                HeadingLevel::H3 => "h3",
                HeadingLevel::H4 => "h4",
                HeadingLevel::H5 => "h5",
                HeadingLevel::H6 => "h6",
            },

            Tag::CodeBlock(_) => "code",

            Tag::List(count_from) => match count_from {
                Some(count_from) => {
                    if count_from != 1 {
                        attrs.push(Attr {
                            name: "start".into(),
                            value: count_from.to_string().into(),
                        });
                    }
                    "ol"
                }
                None => "ul",
            },

            Tag::Item => "li",

            Tag::Table(_ /* TODO: account for alignment */) => "table",

            Tag::TableHead => "thead",

            Tag::TableRow => "tr",

            Tag::TableCell => "td",

            Tag::Emphasis => "em",

            Tag::Strong => "b",

            Tag::Strikethrough => "s",

            Tag::Link { dest_url, title, .. } => {
                if !title.is_empty() {
                    attrs.push(Attr { name: "title".into(), value: String::from(title).into() });
                }
                attrs.push(Attr { name: "href".into(), value: String::from(dest_url).into() });
                "a"
            }

            Tag::Image { dest_url, title, .. } => {
                attrs.push(Attr { name: "src".into(), value: String::from(dest_url).into() });
                if !title.is_empty() {
                    attrs.push(Attr { name: "title".into(), value: String::from(title).into() });
                }
                "img"
            }

            _ => "",
        };
        let mut body = Vec::new();

        while iter.next_if_eq(&end_event).is_none() {
            body.push(Node::parse_markdown(iter));
        }
        if name == "img" && !body.is_empty() {
            attrs.push(Attr {
                name: "alt".into(),
                value: format!("{:#}", Self { body: take(&mut body).into(), ..default() }).into(),
            });
        }

        Self { name: name.into(), attrs: attrs.into(), body: body.into() }
    }

    pub fn attr<'a>(&'a self, name: &str) -> Option<&'a Attr> {
        self.attrs.iter().find(|attr| attr.name == name)
    }

    pub fn attr_mut<'a>(&'a mut self, name: &str) -> Option<&'a mut Attr> {
        self.attrs.iter_mut().find(|attr| attr.name == name)
    }

    pub fn subelements<'a>(&'a self, name: &str) -> impl Iterator<Item = &'a Self> {
        self.body.iter().filter_map(move |x| x.as_element(name))
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Text {
    pub parts: Box<[TextLexeme]>,
    pub indent_level: usize,
}

impl From<StrView> for Text {
    fn from(value: StrView) -> Self {
        Self { parts: [TextLexeme::Text(value)].into(), indent_level: 0 }
    }
}

impl From<&'static str> for Text {
    fn from(value: &'static str) -> Self {
        Self { parts: [TextLexeme::Text(value.into())].into(), indent_level: 0 }
    }
}

impl From<String> for Text {
    fn from(value: String) -> Self {
        Self { parts: [TextLexeme::Text(value.into())].into(), indent_level: 0 }
    }
}

impl Display for Text {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.parts.iter().try_for_each(|part| part.fmt(f))
    }
}

impl Text {
    fn parse(iter: &mut Peekable<impl Iterator<Item = Lexeme>>, indent_level: usize) -> Self {
        let mut parts = Vec::new();

        while let Some(frag) = iter.next_if_map(|frag| match frag {
            Lexeme::Text(text_frag) => Ok(text_frag),
            other => Err(other),
        }) {
            parts.push(frag);
        }

        Self { parts: parts.into_boxed_slice(), indent_level }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Element(Element),
    Text(Text),
}

impl Default for Node {
    fn default() -> Self {
        Self::Text(default())
    }
}

impl From<StrView> for Node {
    fn from(value: StrView) -> Self {
        Self::Text(Text::from(value))
    }
}

impl From<Vec<Self>> for Node {
    fn from(value: Vec<Self>) -> Self {
        match <[Self; 1]>::try_from(value) {
            Ok([only]) => only,
            Err(multiple) => Self::Element(Element { body: multiple.into(), ..default() }),
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Element(element) => element.fmt(f),
            Self::Text(text) => text.fmt(f),
        }
    }
}

impl Node {
    fn parse(
        iter: &mut Peekable<impl Iterator<Item = Lexeme>>,
        indent_level: usize,
        asset: &Asset,
    ) -> Result<Self> {
        if let Some(name) = iter.next_if_map(|lexeme| match lexeme {
            Lexeme::OpeningTagStart(name) => Ok(name),
            frag => Err(frag),
        }) {
            Element::parse(name, iter, indent_level, asset).map(Self::Element)
        } else {
            Ok(Self::Text(Text::parse(iter, indent_level)))
        }
    }

    fn parse_markdown<'a>(iter: &mut Peekable<impl Iterator<Item = Event<'a>>>) -> Self {
        loop {
            let Some(event) = iter.next() else {
                return default();
            };
            break match event {
                Event::Start(tag) => Self::Element(Element::parse_markdown(iter, tag)),
                Event::Text(text) => Self::Text(Text::from(StrView::from_str(&text))),
                Event::Code(code) => Self::Element(Element {
                    name: "code".into(),
                    attrs: default(),
                    body: [StrView::from_str(&code).into()].into(),
                }),
                Event::SoftBreak | Event::HardBreak => {
                    Self::Element(Element { name: "br".into(), ..default() })
                }
                Event::Rule => Self::Element(Element { name: "hr".into(), ..default() }),
                _ => continue,
            };
        }
    }

    pub fn as_element<'a>(&'a self, name: &str) -> Option<&'a Element> {
        match self {
            Self::Element(element) => (element.name == name).then_some(element),
            Self::Text(_) => None,
        }
    }
}

pub struct HtmlParser<'a> {
    iter: Peekable<Lexer<'a>>,
    asset: &'a Asset,
    result: &'a Cell<Result>,
}

impl Iterator for HtmlParser<'_> {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.peek()?;
        Node::parse(&mut self.iter, 0, self.asset)
            .map_err(|e| {
                self.result.set('e: {
                    let Some(ExtraCtx(lexeme)) = e.downcast_ref::<ExtraCtx<Lexeme>>() else {
                        break 'e Err(e);
                    };
                    let Some(src) = self.asset.template_src() else {
                        break 'e Err(e);
                    };
                    let Some(loc) = locate(lexeme.as_src_ptr(), src) else {
                        break 'e Err(e);
                    };
                    Err(e.context(ExtraCtx(FullLocation {
                        path: self.asset.path.to_string().into_path_bytes(),
                        loc,
                    })))
                });
            })
            .ok()
    }
}

impl<'a> HtmlParser<'a> {
    pub fn new(src: StrView, asset: &'a Asset, result: &'a Cell<Result>) -> Self {
        let lexer = Lexer::new(src, asset.clone(), result);
        Self { iter: lexer.peekable(), asset, result }
    }
}

pub struct MarkdownParser<'a> {
    inner: Peekable<pulldown_cmark::Parser<'a>>,
}

impl Iterator for MarkdownParser<'_> {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.peek().is_some().then(|| Node::parse_markdown(&mut self.inner))
    }
}

impl<'a> MarkdownParser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            inner: pulldown_cmark::Parser::new_ext(
                input,
                Options::ENABLE_TABLES | Options::ENABLE_STRIKETHROUGH,
            )
            .peekable(),
        }
    }
}
