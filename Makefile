default: pdf

FILENAME = irods_v5_api_rfc
pdf:
	coqdoc --pdf $(FILENAME).v  -o $(FILENAME).pdf --preamble "\usepackage{tikz}"

clean:
	rm $(FILENAME).pdf
