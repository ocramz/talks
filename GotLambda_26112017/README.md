Building microservices in Haskell
Zimpler, November 29, 2017

## Building the slide deck

The slide template is based on Reveal.js, and uses Highlight.js for syntax highlighting.

Running

    make

in this directory will run `pandoc` on `slides.md`, which in populates the `template-revealjs.html` template file and produces `slides.md`.

All the dependencies are local, so make sure to copy the `css/`, `lib/`, `js/` and `highlight/` directories.


### Requirements

* `pandoc`



