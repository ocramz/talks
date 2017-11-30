slides-pandoc:
	pandoc -t html5 --template=template-revealjs.html --standalone --section-divs --variable transition="linear" --variable theme="black" --variable highlight-style="gruvbox-dark" slides.md -o slides.html

pdf-oneshot:
	pandoc slides.md -o slides.pdf

pdf:
	make latex
	make pdflatex

latex:
	pandoc -t beamer --slide-level=2 --from=markdown --to=latex --standalone slides.md -o slides.tex

pdflatex:
	pdflatex slides.tex
	pdflatex slides.tex


