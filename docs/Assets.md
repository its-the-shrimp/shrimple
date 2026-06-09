# Assets

Assets are files stored alongside the root that'll be copied into the output directory
along with it, with optional processing applied.

## Asset categories
Assets are divided into 4 categories:
- Raw: images, binary data, or text without shrimple templates.
    Raw assets are not processed in any way by the compiler, and are just linked
into the output directory. This is the default asset category.
- Template: text with shrimple templates, but without any HTML. In this type of assets, only
Lua variables & expressions are expanded.
- Document: an asset that exhibits an XML structure, but is not an HTML document. This category of
assets can use shrimple elements, e.g. for defining & expanding templates, as well do everything
template assets can do.
- HTML document:

It's always possible to associate any file extension to any asset category, even if the compiler
already has a category associated to that file extension. By default,
these are the associations `shrimple` defines:
- `.html` (MIME: `text/html`) = HTML document
- `.md` (MIME: `text/markdown`) = HTML document
- `.svg` (MIME: `image/svg`) = Document
- `.css` (MIME: `text/css`) = Template

Files without an asset category associated with their extension are treated as raw assets

To add/remove such an association,
a [`$registerAssetExt`](docs/Elements.md#registerAssetExt) element can be used

## Registering assets
`shrimple` registers assets automatically, by detecting attributes that are known to reference
other files.
Such attributes are called ref(erence) attributes; Find the full list of them below.

To explicitly register an asset if it wouldn't be registered otherwise,
you can use a `link` element just like you would in regular HTML:
```html
<link href=shrimp.png />
```
There is no special element for this, it's that shrimple.

By default, the asset will be assigned a category according to the rule above;
to force template expansion, provide a `$template` attribute before the asset declaration:
```html
<link $template href=shrimp.png />
```
Likewise, to forbid template expansion in the asset, i.e. declare the asset as a raw one,
provide a `$raw` attribute before the asset declaration:
```html
<link $raw href=shrimp.png />
```

To assign the actual path to the asset (relative to website root) to a Lua variable,
place a `$var` attribute before the reference attribute & provide the variable path:
```html
<link $var=SHRIMP_PATH href=shrimp.png />
```

All 3 examples will register `shrimp.png` (relative to the specified root) as an asset

### Reference attributes
Reference attributes are attributes that reference an asset. Such attributes, despite not being
prefixed with `$`, have special meaning for the compiler, as they automatically declare
a new asset during the compilation of the website.

The reference attributes are:
- `<a>`, `<image>`, `<link>`, `<use>`: `href`
- `<img>`, `<script>`: `src`
- `<form>`: `action`

### Caching remote assets
Remote assets are assets that live outside of the current machine, i.e. on a website.
`shrimple` supports caching such assets, so that after compilation they'll essentially become local assets,
i.e. files that live in the output directory.

By default, if an asset path begins with a scheme (e.g. `http://`, `https://`, etc.),
the asset will be ignored & the attribute value won't have any special treatment.

To cache a remote asset provided to a reference attribute, put a `$cached` attribute before it, like so:
```html
<img $cached src="https://raw.githubusercontent.com/its-the-shrimp/shrimple/master/static/shrimple.jpg" />
```
This will compile to roughly something like the following:
```html
<img src="/cached/#.jpg" />
```
where `#` is meaningless gibberish.

Whether the cached asset is a template or a raw asset is determined by the file extension of the link,
or, if the file extension is empty,
by the [`Content-Type`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Content-Type)
HTTP header. To reduce the time wasted on communicating with a remote endpoint, the moment when
a cached asset is found, its remote location is probed with a
[`HEAD`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Methods/HEAD) HTTP request.
Of course, you can always override this by combining a `$raw` or `$template` flag with a `$cached` flag.

### The path
`shrimple` allows for the following simplifications in a path when specified in a ref attribute:
- If a file extension is omitted & the path doesn't end in `/`, `.html` is assumed
- if the file path is empty or is just `/`, `index.html` is assumed
