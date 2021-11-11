CPPC = g++
FC := gfortran
FFLAGS_BASIC = -g -fbacktrace -std=f2008 -pedantic -Wall -Wextra -cpp
FFLAGS_BASIC += -Werror -Werror=shadow -Werror=intrinsic-shadow -Wuninitialized
FFLAGS_BASIC += -Wunreachable-code -Wconversion
FFLAGS_BASIC += -Waliasing -Wampersand -Wc-binding-type -Wcharacter-truncation
FFLAGS_BASIC += -Wfunction-elimination -Wimplicit-interface -Wimplicit-procedure -Wintrinsic-shadow -Wintrinsics-std -Wline-truncation -Wno-tabs
FFLAGS_BASIC += -Wreal-q-constant -Wsurprising
FFLAGS_BASIC += -Wunused-parameter
FFLAGS_BASIC += -Wno-maybe-uninitialized -Wno-unused-dummy-argument -Wno-error=return-type
FFLAGS_BASIC += -Wno-unused-function
FFLAGS_BASIC += -Wno-conversion
FFLAGS_BASIC += -Wno-implicit-interface -Wno-strict-overflow # implicit interface is necessary for calling qsort with general types. Conversions from/to C ints are harmless.

FFLAGS_DEVEL = -O0 -fcheck=all -fbounds-check -Warray-bounds -Wstrict-overflow=5 -Wunderflow -ffpe-trap=invalid,zero,overflow
# FFLAGS_DEVEL += -ftrapv
FFLAGS_RELEASE = -O3

# not yet in gfortran 4.8.5:
# FFLAGS_BASIC += -Wdo-subscript  -std=f2018  -Wfrontend-loop-interchange
# FFLAGS_DEVEL += -fsanitize-address-use-after-scope

# CPPC = icpc
# FC = ifort
# FFLAGS_BASIC = -g -traceback -cpp
# FFLAGS_DEVEL = -O0
# FFLAGS_RELEASE = -Ofast

FFLAGS = $(FFLAGS_DEVEL) $(FFLAGS_BASIC)

.PHONY: all test clean

all: test

test: fhash_modules fhash_test.f90
	$(FC) $(FFLAGS) fhash_modules.f90 fhash_test.f90 -o fhash_test.out  \
        &&   ./fhash_test.out

benchmark: fhash_benchmark.out stl_benchmark.out
	./fhash_benchmark.out  &&  ./stl_benchmark.out

fhash_benchmark.out: fhash_modules.f90 benchmark.f90
	$(FC) $(FFLAGS_BASIC) $(FFLAGS_RELEASE) fhash_modules.f90 benchmark.f90 -o fhash_benchmark.out

stl_benchmark.out: benchmark.cc
	 $(CPPC) -std=c++11 -O3 $< -o $@

clean:
	rm -rf *.mod *.o

fhash_modules: fhash.f90 fhash_modules.f90
	$(FC) $(FFLAGS) -c fhash_modules.f90
