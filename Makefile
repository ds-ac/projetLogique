# The initial project Makefile has been modified with the help of Neven's one.

# Add binary names to BINARIES as you progress through the project:
# they can be problem encoders or SAT solvers; in both cases you must
# include the extension TODO

BINARIES_NOSKEL=
BINARIES=latin jr greek naive arrays twl $(BINARIES_NOSKEL)
OCAMLOPT = ocamlopt -I src -I target

# Define non-files targets
.PHONY: skel clean all mrproper doc

all: $(BINARIES) doc



# Targets for compiling both problem encoders and SAT solvers
%: target/%.cmx
	$(OCAMLOPT) $(shell cat .depends | grep "^$+" | cut -d: -f2) $+ -o $@

target/%.cmx: src/%.ml |target
	$(OCAMLOPT) -c $< -o $@

target/%.cmi: src/%.mli |target
	$(OCAMLOPT) -c $< -o $@

target:
	mkdir -p target

sat_test:
	mkdir sat_test

-include .depends
.depends: Makefile $(wildcard src/*.ml src/*.mli)
	ocamldep -one-line -native -I src $+ | sed 's,src,target,g' > .depends


# Testing problem encodings to SAT using minisat
N=10
test_latin: latin sat_test
	./latin p $(N)
	minisat problem.cnf output.sat ; ./latin s $(N)
	@mv problem.cnf output.sat sat_test/

test_greek: greek sat_test
	./greek p $(N)
	minisat problem.cnf output.sat ; ./greek s $(N)
	@mv problem.cnf output.sat sat_test/

test_wang: jr sat_test
	./jr p $(N)
	minisat problem.cnf output.sat ; ./jr s $(N)
	@mv problem.cnf output.sat sat_test/

PROBLEM=problems/0/simple1
test_pingouins: pingouins
	./pingouins p $(PROBLEM)
	minisat problem.cnf output.sat ; ./pingouins s $(PROBLEM)
PENALTY=0
tests_pingouins: pingouins
	for i in problems/$(PENALTY)/* ; do \
	  make PROBLEM=$$i PENALTY=$(PENALTY) test_pingouins ; \
	done

# Testing the SAT solver

PROVER=./twl
in_test: all sat_test
	@for i in tests/SAT/* ; do \
		tmp="$${i##tests/SAT}" ; \
		new="sat_test$${tmp%cnf}sat" ; \
		printf "%42s -> %s\r%32s ... " \
			"" "$${new}" "$$i" ; \
		$(PROVER) $$i "$$new" ; \
		grep -v UNSAT "$$new" > /dev/null || \
			(printf "\e[01;31mUNSAT\e[0m\n\n"; exit 1) ; done
	@for i in tests/UNSAT/* ; do \
		tmp="$${i##tests/UNSAT}" ; \
		new="sat_test$${tmp%cnf}sat" ; \
		printf "%42s -> %s\r%32s ... " \
			"" "$${new}" "$$i" ; \
		$(PROVER) $$i "$$new" ; \
		grep -e UNSAT "$$new" > /dev/null || \
			(printf "\e[01;31mSAT\e[0m\n\n"; exit 1) ; done

test: all sat_test
	@echo Timing tests with minisat...
	@time --output=tests/minisat.time --format=%U \
	  make PROVER=minisat in_test > /dev/null
	@printf "\e[32m"
	@cat tests/minisat.time
	@printf "\e[0m"
	@echo Timing tests with $(PROVER)...
	@time --output=tests/prover.time --format=%U make in_test
	@printf "\e[32m"
	@cat tests/prover.time
	@printf "\e[0m"
	@m=`cat tests/minisat.time` ; p=`cat tests/prover.time` ; \
	  echo -n "Ratio: " ; echo "$$p / $$m" | bc



# Cleaning, documentation, code skeleton
clean:
	rm -rf target sat_test

mrproper: clean
	rm -f $(BINARIES)

doc:
	ocamldoc -d html/ -stars -html src/dimacs.mli src/hex.mli

skel:
	rm -rf skel skel.test
	mkdir -p skel/src skel/html
	cat Makefile | sed -e 's/BINARIES_NOSKEL=
	  > skel/Makefile
	cp -r problems/ skel/
	cp src/dimacs.ml* src/hex.ml* src/latin.ml src/naive.ml skel/src/
	cp src/tile_*.ml skel/src/
	cp html/style.css skel/html/
	cp -r tests skel/
	cp -r skel/ skel.test/
	make -C skel.test

