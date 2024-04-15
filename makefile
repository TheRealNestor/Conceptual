make:
	opam update
	opam install . --deps-only	
	dune build

build:
	dune build

install:
	opam update
	opam install --yes . --deps-only

.PHONY: test
test:
	dune runtest	
	

clean:
	dune clean

# Use the second word in the MAKECMDGOALS as the RUN_ARG (first word is run)
# This tells make not to complain about the RUN_ARG not being a real target
ifeq (run,$(firstword $(MAKECMDGOALS)))
  RUN_ARG := $(word 2, $(MAKECMDGOALS))
  $(eval $(RUN_ARG):;@:)
endif
# Beware that vscode is configured to use tabs for indentation in makefiles, but scripting requires space indentation

run:
	dune exec ./bin/main.exe $(RUN_ARG)

#Ensure that the grammar rule is always run
# https://github.com/Lelio-Brun/Obelisk?tab=readme-ov-file
.PHONY: grammar
grammar:
	rm -f grammar/*
	obelisk latex src/parser.mly > grammar/obelisk.tex
	sed -i '/\\documentclass/a \\n\\\usepackage[right=0.1cm]{geometry}' grammar/obelisk.tex
	pdflatex -output-directory=grammar grammar/obelisk.tex
