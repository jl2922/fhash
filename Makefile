FC := gfortran
FFLAGS := -O3 -g -Wall -Wextra -cpp -Wno-unused-dummy-argument

.PHONY: all test clean

all: test

test: fhash_modules fhash_test.f90
	$(FC) $(FFLAGS) fhash_modules.f90 fhash_test.f90 -o fhash_test && ./fhash_test

clean:
	rm -rf *.mod *.o

fhash_modules: fhash.f90 fhash_modules.f90
	$(FC) $(FFLAGS) -c fhash_modules.f90