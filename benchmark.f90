#define KEY_ARRAY_SIZE 2

#define FHASH_MODULE_NAME int_intsptr_fhash_mod
#define FHASH_TYPE_NAME int_intsptr_fhash_type
#define FHASH_TYPE_ITERATOR_NAME int_intsptr_fhash_iter_type
#define KEY_TYPE integer, dimension(KEY_ARRAY_SIZE)
#define KEY_IS_ARRAY
#define VALUE_TYPE real(real64)
#define VALUE_USE use, intrinsic :: iso_fortran_env, only: real64
! #define VALUE_TYPE_INIT null()
! #define VALUE_POINTER
#include "fhash.f90"

program test_benchmark
  implicit none
  
  real :: start, finish

  print *, 'Start benchmark:'
  call cpu_time(start)
  call benchmark(n_ints = KEY_ARRAY_SIZE, n_keys=10000000)
  call cpu_time(finish)
  print '("Time finish = ", G0.3," seconds.")', finish - start
  
  contains
  subroutine benchmark(n_ints, n_keys)
    use int_intsptr_fhash_mod
    use, intrinsic :: iso_fortran_env, only: real64

    integer, intent(in) :: n_ints, n_keys

    type(int_intsptr_fhash_type) :: h
    integer :: key(n_ints)
    real :: start, finish
    integer :: i, j

    print '("n_ints: ", I0, ", n_keys: ", I0)', n_ints, n_keys

    call cpu_time(start)
    call h%reserve(n_keys * 2)
    do i = 1, n_keys
      do j = 1, n_ints
        key(j) = i + j
      enddo
      call h%set(key, (i + j) * 0.5_real64)
    enddo
    call cpu_time(finish)
    print '("Time insert = ", G0.3," seconds.")', finish - start
    call h%clear()
  end subroutine
end program
