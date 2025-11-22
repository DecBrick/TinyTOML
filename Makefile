FC=gfortran
FCFLAGS= -g -fbacktrace -fcheck=all -Og
SRC=\
	src/tinytoml.f90 \
	test/fort_test.f90 \
	test/runtests.f90


PROGRAM=toml-test.exe
PRG_OBJ=$(PROGRAM).o

.PHONY: test

test: $(PROGRAM)
	./$(PROGRAM)

$(PROGRAM): $(SRC)
	$(FC) $(FCFLAGS) $(FLFLAGS) -o $(PROGRAM) $(SRC)

clean:
	rm -rf $(PROGRAM) *.mod *.dSYM *.exe
