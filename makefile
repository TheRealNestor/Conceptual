# Could optionally add --inspection flag here
# insepection flag can only be used when table flag is used. 
make:
	dune clean
	dune build

clean:
	dune clean

# Use the second word in the MAKECMDGOALS as the RUN_ARG (first word is run)
# This tells make not to complain about the RUN_ARG not being a real target
ifeq (run,$(firstword $(MAKECMDGOALS)))
  RUN_ARG := $(word 2, $(MAKECMDGOALS))
  $(eval $(RUN_ARG):;@:)
endif
# Beware that vscode is configured to use tabs for indentation in makefiles, but scripting requires space indentation

# dune exec ./conceptual.bc $(RUN_ARG)
# above is for bytecode compilers
run:
	dune exec ./conceptual.exe $(RUN_ARG)

#Ensure that the grammar rule is always run
# https://github.com/Lelio-Brun/Obelisk?tab=readme-ov-file
.PHONY: grammar
grammar:
	rm -f grammar/*
	obelisk latex parser.mly > grammar/obelisk.tex
	sed -i '/\\documentclass/a \\n\\\usepackage[right=0.1cm]{geometry}' grammar/obelisk.tex
	pdflatex -output-directory=grammar grammar/obelisk.tex
