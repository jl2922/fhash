FC := gfortran
FFLAGS := -O3 -Wall -Wextra

SRC_FILE := fhash.f90
TEST_FILE := fhash_test.f90
TEST_OUTPUT := fhash_test

all: $(SRC_FILE)
	$(FC) $(FFLAGS) -c $<

test: ./$(TEST_OUTPUT)
	./$(TEST_OUTPUT)

$(TEST_OUTPUT): $(SRC_FILE) $(TEST_FILE)
	$(FC) $(FFLAGS) $^ -o $@