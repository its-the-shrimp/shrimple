# Assets

Assets are files stored alongside the root that'll be copied into the output directory
along with it, with optional template expansion applied.

## Asset categories
Assets are divided into 2 categories:
- Raw assets: images, binary data, or text without shrimple templates.
    Raw assets are not processed in any way by the compiler, and are just linked
into the output directory.
- Template assets: text (not necessarily an HTML document) with shrimple templates.
    HTML is a special case among the template assets, since it has its 

It's always possible to specify whether a given asset is raw or a template, but by default only files
with the following extensions/MIME type are templates:
- `.html` (MIME: `text/html`)
- `.css` (MIME: `text/css`)
- `.svg` (MIME: `image/svg`)
- `.md` (MIME: `text/markdown`)

Files with any other extension are treated as raw assets.

To add a file extension to the list of template asset extensions,
a [`$registerTemplateExt`](docs/Elements.md#registerTemplateExt) element can be used:

## Registering assets
`shrimple` registers assets automatically, by detecting attributes that are known to reference
other files.
Such attributes are called ref(erence) attributes; Find the full list of them below.

To explicitly register an asset if it wouldn't be registered otherwise, a `$ref` element can be used:
```html
<$ref $SHRIMP=shrimp.png />
```

By default, it'll be assigned a category according to the rule above;
to force template expansion, provide a `$template` attribute before the asset declaration:
```html
<$ref $template $SHRIMP=shrimp.png />
```
Likewise, to forbid template expansion in the asset, i.e. declare the asset as a raw one,
provide a `$raw` attribute:
```html
<$ref $raw $SHRIMP=shrimp.png />
```

All 3 examples will register `shrimp.png` (relative to the specified root) as an asset
and create a variable `SHRIMP` with the path to `shrimp.png`

`$raw` & `$template` are also allowed in regular elements before reference attributes.
```html
<a $raw href=stuff>Rawdogging it</>
```

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
<img $cached src="https://raw.githubusercontent.com/schvv31n/shrimple/master/static/shrimple.jpg" />
```
This will compile to roughly something like the following:
```html
<img src="/cached/#.jpg" />
```
where `#` is meaningless gibberish.

To cache a remote asset explicitly registered with `$ref`, put a `$cached` attribute before it, like so:
```html
<$ref $cached $SHRIMP="https://raw.githubusercontent.com/schvv31n/shrimple/master/static/shrimple.jpg" />
```
This will cache the content at the URL and assign to the variable `SHRIMP` a filename in the form of `/cached/#.jpg`
where the `#` is, yet again, meaningless gibberish.

Whether the cached asset is a template or a raw asset is determined by the file extension of the link,
or, if the file extension is empty,
by the [`Content-Type`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Content-Type)
HTTP header. To reduce the time wasted on communicating with a remote endpoint, the moment when
a cached asset is found, its remote location is probed with a
[`HEAD`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Methods/HEAD) HTTP request.
Of course, you can always override this by combining a `$raw` or `$template` flag with a `$cached` flag.

