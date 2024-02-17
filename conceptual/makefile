# Could optionally add --inspection flag here
# insepection flag can only be used when table flag is used. 
make:
	rm -f parser.ml parser.mli
	dune build
	menhir --explain --inspection --table --dump parser.mly

clean:
	rm -f parser.ml parser.mli
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
	rm -f parser.ml parser.mli
	dune exec ./conceptual.exe $(RUN_ARG)



