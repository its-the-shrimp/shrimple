## Shrimple HTML

`shrimple` utilises a subset of HTML, with the following fundamental restriction: <br />
All elements must be either self-closing, or have a closing tag
- Self-closing element:
```html
<!DOCTYPE html />
```
- Element with a closing tag:
```html
<b>It's as shrimple as that</b>
```
This brings consistency to the infamously irregular HTML syntax & makes the resulting DOM more predictable.
Being a subset of HTML means that all existing tooling for syntax highlighting
& auto-completion in HTML can be used just as efficiently 
for shrimple templates.
Because arbitrary text is valid HTML and is treated as a text node, shrimple templates can be used in a text file of any format:
CSS, JS, etc. (But be aware of possible overlaps in the syntax of the file & of shrimple templates)

## The important parts
There are 2 kinds of things that are significant to the compiler:
1. Everything prefixed with `$`:
    - Elements prefixed with `$`,
    - Attributes in normal elements prefixed with `$`.
2. [Reference attributes](docs/Assets.md#Reference_attributes)


All elements that are processed during building have their names prefxied with `$`. Those will get replaced with the contents they evaluate it to:
- `<$lua>` will expand to the return value of the Lua code;
- `<$ref>` will expand to nothing (as it's used only for defining a variable);
- any defined template will expand to its children.

## The `$`
A stray dollar sign in an attribute value or in free text is a shortcut for a `<$lua>` element. <br />
`$VAR` expands to the value of the variable `VAR`, equivalent to `<$lua>VAR</$lua>` <br />
`$(...)` expands to the return value of the Lua code inside the parenthese, equivalent to `<$lua>...</$lua>` <br />

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
