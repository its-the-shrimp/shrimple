# Shrimple elements

All elements whose names begin with `$` have special meaning. Those can be built-in shrimple elements
or user-defined templates. Below is the list of all built-in elements along with their signature
& behaviour

All elements have 3 ways of accepting children:
- forbidden
- optional
- required

The symbols after the parameter names denote how many times must a parameter be provided:
- `(1)` - Exactly once;
- `(*)` - 0 or more;
- `(+)` - 1 or more;
- `(?)` - 0 or 1

## `<$children>`

```html
<$template name=CODE acceptsChildren>
    <$raw code>
        <$children />
    </>
</>
```

Expands to the children provided to the currently expanded template, thus forbidden outside of `<$template>`

**Children:** forbidden

## `<$foreach>`

```html
<$foreach $POST in dir=posts>
    <div class=post>
        <a href=$POST>Post $index</>
    </>
</>
```

Iterates a directory and expands to its children for every encountered file.

On every iteration sets the variable `index` to the number of the current iteration, starting from 0

### Parameters (exactly in the specified order):
- `$VAR_NAME (1)`: The variable which will have the path to the file on the current step of the iteration.
- `in (1)`: Just a separator for more readability.
- `dir=path (1)`: `path` is the directory to be iterated over.

**Children:** required

## `<$raw>`

Escapes all the special HTML symbols in its children, rendering the resulting HTML as literal text.

Allows passing attributes to replace `<$raw>` with another element.

The following snippet:
```html
<$raw code class=cool-code>
    <b>This is code now</>
</>
<$raw>
    <b>And this is just text</>
</>
```
Will expand to the following:
```html
<code class="cool-code">
    &lt;b&gt;This is code now&lt;/b&gt;
</code>
&lt;b&gt;And this is just text&lt;/b&gt;
```

### Parameters:
- `name attrs... (?)`: replace `$raw` with `name` as the element name and keep `attrs...` in it.

**Children:** required

## `<$ref>`

```html
<$ref processed $JS="index.js" />
```

Manually register an asset & assign the path to it (unchanged) to a Lua variable.

Keep in mind that if the asset is mentioned in an HTML element's attribute, this is unnecessary.

**Parameters:**
- `$VAR_NAME=path (1)`: Register `path` as an asset and assign `path` to the variable `VAR_NAME`. `path` must resolve to the path to a local file.
- `$raw (?)`: Enforce that the asset is _not_ processed as a template file. Mutually exclusive with `$template`.
- `$template (?)`: Enforce that the asset is processed as a template file. Mutually exclusive with `$raw`.

If neither `$raw` nor `$template` are specified,
processing mode of the asset is the determined by its file extension.
See the [Assets](docs/Assets.md) section for more information on this. <br />

**Children:** forbidden

## `<$registerTemplateExt>`

```html
<$registerTemplateExt js />
```

Registers a file extension, the files with which will be processed as template files.

The example above declares that from then on, `.js` files are to be treated as template assets.

When registering a new template file extension, make sure the syntax of corresponding files doesn't
overlap with the syntax of shrimple templates, namely, pay attention to the way `$` is treated

### Parameters:
- `ext (1)`: `ext` will be the new processed file extension. It must not contain the initial dot.

**Children:** forbidden

## `<$template>`

```html
<$template name=BODY acceptsChildren $title="Insert text">
    <div class=fg>
        <$children />
    </>
</>
```

Define a new template.

### Parameters:
- `name=name (1)`: `name` is the name of the template.
- `acceptsChildren (?)`: If provided, the template will optionally accept children, otherwise it'll reject them.
- `$ARG_NAME (*)`: `ARG_NAME` is a required parameter to the template, and will be a variable while the template is expanded.
- `$ARG_NAME=default (*)`: `ARG_NAME` is an optional parameter to the template, and will be a variable while the template is expanded. If not provided, it'll be assigned the value `default`.

**Children:** optional
