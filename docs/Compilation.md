# Compilation

### Terminology
In an invocation of `shrimple`:
```console
shrimple -o dist index.html
```
`index.html` is _the root_, and `dist` is the _output directory_.
All references to local assets are considered to be relative to the root.

### The process:
1. The asset is parsed into lexemes
2. Lexemes are gathered into a Concrete Syntax Tree (CST)
3. The CST is processed downward & inward, evaluating Lua code, variable access,
template declarations, expansions, etc. in that order.
4. If the asset is HTML, it goes through post-processing steps, where the following transformations
are applied:
    - `<!DOCTYPE html>` gets added to the top of the document, if absent originally;
    - The document is wrapped in `<html>`, if not wrapped originally;
    - A `<body>` & a `<head>` are added to `<html>`, if absent originally;
    - All direct children of `<html>` that are [head elements] get wrapped in `<head>`,
        the rest get wrapped in `<body>`, if it was absent originally;
    - `<meta charset="UTF-8" />` is inserted into `<head>`, if absent originally.
5. Steps 1-4 get applied to all the other template assets after the root. If an asset declares
a new template asset, that asset gets into a queue that is advanced only when the current asset
is fully compiled, i.e. the compilation sequence is flat & sequential.
6. After all the assets are discovered & all template assets are compiled, the results are written
to the output directory. Raw assets don't get copied, but are rather hard linked into the output
directory.

Step 5 implies that you can reduce boilerplate by omitting `<head>`, `<html>`, `<!DOCTYPE>`
& `<meta charset="UTF-8">` from your shrimple HTML.
Step 5 also implies that an empty HTML file will compile down to the following structure:
```html
<!DOCTYPE html>
<html>
    <head>
        <meta charset="UTF-8" />
    </head>
    <body></body>
</html>
```

[head elements]: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/head#see_also
