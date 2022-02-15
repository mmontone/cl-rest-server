.PHONY: html clean

html:
	makeinfo --html --split=chapter --css-include manual.css cl-rest-server.texi

pdf:
	makeinfo --pdf cl-rest-server.texi

clean:
	rm -f cl-rest-server/*
