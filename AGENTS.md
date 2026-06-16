# shrimple website generation guide

`shrimple` compiles `.html` and `.md` files into a static website. Run it as:
```console
shrimple -o dist index.html
```

---

## Write shrimple HTML, not plain HTML

Skip all boilerplate — no `<!DOCTYPE>`, `<html>`, `<head>`, `<body>`, or `<meta charset>`. The compiler inserts them automatically. Just write your content:

```html
<title>My Site</>
<link rel=stylesheet href=style.css />

# Hello, World
Welcome to my site.
```

Close tags by omitting the element name — `</>` instead of `</div>`, etc. This is preferred throughout.

Omit quotes around attribute values if the value doesn't have whitespace, `/>` or `>`

---

## Mix Markdown and HTML freely

All text nodes support Markdown (CommonMark + extras). Use Markdown for prose and HTML for structure:

```html
<article>
    # Section Title

    Some **bold** and *italic* text, plus ~strikethroughs~.

    | Col A | Col B |
    |-------|-------|
    | 1     | 2     |
</article>
```

Use HTML when you need semantic tags or attributes; use Markdown for everything else.

---

## Use `$` for variables and Lua expressions

Set and use variables with `$`:

```html
$(SITE_NAME = "Acme Corp")
<title>$SITE_NAME</>
<h1>Welcome to $SITE_NAME</>
```

Run arbitrary Lua inline with `$(...)`:

```html
<p>Page built on $(os.date("%Y-%m-%d"))</>
<div class="$(is_home and 'hero' or 'page')">...</>
```

Variables work inside attribute strings too:

```html
<div class="card $EXTRA_CLASS">...</>
```

---

## Define and reuse templates

Use `<$template>` to create reusable components. Reference them as `<$NAME>`:

```html
<$template name=Card $title $href $desc="No description">
    <article class=card>
        ## [$title]($href)
        $desc
    </>
</>

<$Card title=Intro href=/about desc="Learn more about us" />
<$Card title=Blog href=/blog />
```

Templates that wrap other content use `acceptsChildren` and `<$children />`:

```html
<$template name=Section $heading acceptsChildren>
    <section>
        ## $heading
        <$children />
    </>
</>

<$Section heading=Features>
    - Fast
    - Simple
    - Shrimple
</>
```

---

## Iterate directories with `<$foreach>`

Loop over files in a directory to generate repeated sections automatically:

```html
<$foreach $POST in dir=posts>
    <a href=$POST>Post $(index + 1)</>
</>
```

Files are visited in alphabetical order. `index` is 0-based.

---

## Use `<$raw>` to render code examples

Wrap HTML snippets you want displayed as literal text:

```html
<$raw code class=snippet>
    <b>This won't be rendered as bold</>
</>
```

---

## Register and reference assets

Any ref attribute (`href`, `src`, `action`) automatically registers the file as an asset and copies it to the output:

```html
<img src=logo.png />
<script src=app.js></>
<a href=about>About</a>       <!-- .html is assumed -->
<a href=/>Home</a>             <!-- resolves to index.html -->
```

Force template expansion (so `$` variables work) with `$template`:

```html
<link $template href=vars.css />
```

Capture the output path to a variable with `$var`:

```html
<link $var=LOGO_PATH href=logo.png />
<meta property="og:image" content=$LOGO_PATH />
```

Cache a remote file locally with `$cached`:

```html
<img $cached src=https://example.com/banner.jpg />
```

---

## CSS is a template by default

`.css` files support `$VAR` and `$(...)` out of the box — no extra setup needed:

```css
/* style.css */
$(PRIMARY = "#4f46e5")
body { background: $PRIMARY; }
a    { color: $(PRIMARY); }
```

---

## Keep content files clean with `$wrapIn`
 
Don't put navbars, footers, or other shared layout markup inside content files.
Define that structure once as a template, then attach it to a ref attribute with `$wrapIn`
so the content file stays pure content:
 
```html
<$template name=withNavBar acceptsChildren>
    <nav>...</>
    <$children />
</>
 
<a $wrapIn=withNavBar href=post.html>A post</>
```
 
The template named in `$wrapIn` must accept children and have no required parameters.
Reach for this whenever a page is mostly content but still needs to live inside a shared layout
— it keeps content files DRY and separates structure/presentation concerns from the content itself,
which matters most as a site grows or multiple files share the same layout.
 
---

## Checklist when writing a site

- Skip all HTML boilerplate — let the compiler add it
- Use `</>` to close every tag
- Write prose and lists in Markdown; use HTML only for structure
- Define shared layouts/components as `<$template>`
- Use `<$foreach>` for any list driven by files (posts, products, team members…)
- Synchronise HTML & CSS by urilising `$` variables
- Cache any external images or fonts with `$cached`
- Omit `.html` from internal links
