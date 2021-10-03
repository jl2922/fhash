FC := gfortran
FFLAGS = -g -fbacktrace -std=f2018 -pedantic -Wall -Wextra -cpp -Wno-unused-dummy-argument
FFLAGS += -Werror -Werror=shadow -Werror=intrinsic-shadow -Wuninitialized
FFLAGS += -Wunreachable-code -Wconversion
FFLAGS += -Wno-maybe-uninitialized -Wno-unused-dummy-argument -Wno-error=return-type
FFLAGS += -Waliasing -Wampersand -Wc-binding-type -Wcharacter-truncation -Wconversion
FFLAGS += -Wno-unused-function
FFLAGS += -Wdo-subscript -Wfunction-elimination -Wimplicit-interface -Wimplicit-procedure -Wintrinsic-shadow -Wintrinsics-std -Wline-truncation -Wno-tabs
FFLAGS += -Wreal-q-constant -Wsurprising
FFLAGS += -Wunused-parameter -Wfrontend-loop-interchange

FFLAGS_DEVEL = -O0 -fcheck=all -fbounds-check -Warray-bounds -Wstrict-overflow=5 -Wunderflow -fsanitize-address-use-after-scope -ffpe-trap=invalid,zero,overflow
# FFLAGS_DEVEL += -ftrapv
FFLAGS_RELEASE = -O3
FFLAGS += $(FFLAGS_RELEASE)

.PHONY: all test clean ref

all: test

test: fhash_modules fhash_test.f90
	$(FC) $(FFLAGS) fhash_modules.f90 fhash_test.f90 -o fhash_test.out  \
        &&   ./fhash_test.out

ref: benchmark.cc
	g++ -O3 -std=c++14 benchmark.cc -o ref.out && ./ref.out

clean:
	rm -rf *.mod *.o

fhash_modules: fhash.f90 fhash_modules.f90
	$(FC) $(FFLAGS) -c fhash_modules.f90
