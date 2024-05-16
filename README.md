![It's as shrimple as that](https://raw.githubusercontent.com/schvv31n/shrimple/master/static/shrimple.jpg)
# The shrimple static site generator
## Installation
```console
cargo install shrimple
```
## Features
### 1. Keep it shrimple
No external configuration needed: you have HTML, you have `shrimple`, you call it, and get a site ready to be deployed.
```console
shrimple -o dist index.html
```
This will build the website with `index.html` as its root and paste the root and all the files it references into `dist`
### 2. Compute anything anywhere with Lua
`shrimple` has Lua evaluation built-in and it can be utilised in any part of any file: in text nodes, in attributes, inside strings, you name it!
Use `$VAR` to access a Lua variable, and `$(code)` or `<$lua>code</$lua>` to evaluate arbitrary Lua code and paste its return value
```html
<$lua>
  SHRIMP = "shrimp.png"
  SHRIMP_SIZE = 100
</$lua>
<svg width=$SHRIMP_SIZE height=100%>
  <image
    href=$SHRIMP
    width=$SHRIMP_SIZE height=$SHRIMP_SIZE
    transform="rotate(-30 $(SHRIMP_SIZE / 2) $(SHRIMP_SIZE / 2))"
  />
</svg>
```
### 3. Don't list assets, just use them
No need to deliberately specify `index.css`, `image.png`, etc. as an asset: if you mention it in HTML, it'll be registered automatically.
In the example above, the file `shrimp.png` is mentioned in `image` element's `href` attribuet, so it will automatically be searched
in the same directory where `index.html` is, and will be copied to the output directory.

If the asset is somewhere out there on the vast plains of the Internet, and you wish to make sure it's always available to your users,
you can cache it! just prefix the attribute containing the link with `$cached`, and you're all set.

### 4. Iterate quickly
The CLI comes with a very handy flag: call `shrimple -w` or `shrimple --watch` to spin up a lightweight local server
that'll recompile your website as needed and show it to you right in the browser.
