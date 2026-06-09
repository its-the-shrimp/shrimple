## Shrimple HTML

`shrimple` utilises a subset of HTML, with the following differences:
- All elements must be either self-closing, or have a closing tag
    - Self-closing element:
    ```html
    <!DOCTYPE html />
    ```
    - Element with a closing tag:
    ```html
    <b>It's as shrimple as that</>
    ```
    This brings consistency to the infamously irregular HTML syntax & makes the resulting DOM more predictable.
- The element name can be omitted from the closing tag
    ```html
    <code>This</> is the same as <code>this</code>
    ```
    This reduces visual noise, makes it faster to write shrimple HTML, and saves on LLM tokens.
- No more boilerplate
    `<head>`, `<body>`, `<html>`, `<meta charset="UTF-8">` & `<!DOCTYPE html>`,
        the modern HTML mantra, doesn't have to be typed out at all: instead, it's inserted by the
        compiler.
    ```html
    <title>This is a title</>
    This is a <s>free text</> body
    ```
    becomes
    ```html
    <!DOCTYPE html />
    <html>
        <head>
            <meta charset="UTF-8" />
            <title>This is a title</title>
        </head>
        <body>
            This is a <s>free text</> body
        </body>
    </head>
    ```
    The compiler knows which elements should go into `<head>` and which into `<body>`, and specifies
    that the document is standard HTML in the UTF-8 encoding
- `Markdown + HTML = <3`
    All HTML files will have their Markdown compiled down to HTML elements
    ```markdown
    # This is **readable** text
    And people will *love* you for it
    ```
    becomes
    ```html
    <h1>This is <b>readable</b> text</h1>
    And people will <s>hate</s> <em>love</em> you for it
    ```

Because arbitrary text is valid HTML and is treated as a text node, shrimple templates can be used in a text file of any format:
CSS, JS, etc. (But be aware of possible overlaps in the syntax of the file & of shrimple templates)

## The important parts
There are 2 kinds of things that are significant to the compiler:
1. Everything prefixed with `$`:
    - Elements prefixed with `$`,
    - Attributes in normal elements prefixed with `$`.
2. [Reference attributes](Assets.md#Reference_attributes)

All elements that are processed during building have their names prefxied with `$`.
Those will get replaced with the contents they evaluate to.

## The `$`
`$VAR` expands to the value of the variable `VAR`.

`$(...)` expands to the return value of the Lua code inside the parentheses.

An example of `$` syntax in HTML attribute value position and in free text:
```html
$(CLASS = "cool-class")
<div class=$CLASS>$( "I'm styled with a class " .. CLASS )</div>
```

An example of `$` syntax inside a string literal value of an HTML attribute:
```html
$(CLASS = "cool-class")
<div class="$CLASS another-cool-class">Hello, World!</div>
```

## Markdown

Shrimple markdown is based on the [CommonMark](https://commonmark.org/) standard with the following differences:
- Striktethroughs are supported, which are not part of CommonMark, but are a very common extension.
    `~abc~` becomes `<s>abc</s>`
- Tables are supported, which are also a common extension to Markdown
    ```markdown
    | header row 1 | header row 2 | header row 3 |
    |--------------|--------------|--------------|
    | value 1      | value 2      | value 3      |
    ```
    becomes
    ```html
    <table>
        <thead>
            <td>header row 1</td>
            <td>header row 2</td>
            <td>header row 3</td>
        </thead>
        <tr>
            <td>value 1</td>
            <td>value 2</td>
            <td>value 3</td>
        </tr>
    </table>
    ```

The markdown in every text node is compiled assuming that there is indentation
in the markdown to account for its depth in the HTML.
The standard indentation step in `shrimple` is 4 spaces.
Thus, the following piece of shrimple HTML:
```html
<div>
    - a
    - b
    - c
</>
```
will be compiled as if the text there were
```markdown
- a
- b
- c
```
without the indentation, which could've meddled with the interpretation of the markdown.
