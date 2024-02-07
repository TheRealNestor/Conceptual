make:
	dune build
	menhir --explain --inspection --table --dump parser.mly

ifeq (run,$(firstword $(MAKECMDGOALS)))
  # Use the second word in the MAKECMDGOALS as the RUN_ARG (first word is run)
  RUN_ARG := $(word 2, $(MAKECMDGOALS))
  # This tells make not to complain about the RUN_ARG not being a real target
  $(eval $(RUN_ARG):;@:)
endif

run: 
	dune exec ./conceptual.exe $(RUN_ARG)



