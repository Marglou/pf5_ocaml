opam_exec := opam exec --switch $$PWD --

.PHONY : all
all : build

.PHONY : phony
phony :

.PHONY : build
build :
	@ $(opam_exec) dune build
	@ $(MAKE) interpreteur_tutorial # affiche les options lors du build


.PHONY : interpreteur_tutorial  # execute le fichier interpreteur_tutorial.sh affichant les options
interpreteur_tutorial :
	@./src/interpreteur_tutorial.sh

.PHONY : test
test :
	@ $(opam_exec) dune exec test/test.exe -- test
test-% : phony
	@ $(opam_exec) dune exec test/test.exe -- test $*

.PHONY : top
top :
	@ $(opam_exec) dune utop . -- -init top.ml

.PHONY : clean
clean :
	@ $(opam_exec) dune clean

.PHONY : run
run :
	@ $(opam_exec) dune exec main/main.exe -- $(ARGS)
	# make run ARGS="-size 800x600 -bc 255,0,0"
