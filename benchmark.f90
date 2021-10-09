#define KEY_ARRAY_SIZE 2

#define FHASH_MODULE_NAME int_intsptr_fhash_mod
#define FHASH_TYPE_NAME int_intsptr_fhash_type
#define FHASH_TYPE_ITERATOR_NAME int_intsptr_fhash_iter_type
#define KEY_TYPE integer, dimension(KEY_ARRAY_SIZE)
#define VALUE_TYPE real(real64)
#define VALUE_USE use, intrinsic :: iso_fortran_env, only: real64
! #define VALUE_TYPE_INIT null()
! #define VALUE_POINTER
#include "fhash.f90"

program test_benchmark
  implicit none
  
  call benchmark(n_ints=KEY_ARRAY_SIZE, n_keys=10000000)
  
contains
  subroutine benchmark(n_ints, n_keys)
    use int_intsptr_fhash_mod

    integer, intent(in) :: n_ints, n_keys

    type(int_intsptr_fhash_type) :: h
    integer :: key(n_ints)
    integer :: i, j
    real :: t0, t1, t2

    write(*,'(a)') "Start fhash benchmark:"

    write(*,'("n_ints: ", I0, ", n_keys: ", I0)') n_ints, n_keys

    call cpu_time(t0)
    call h%reserve(n_keys * 2)
    do i = 1, n_keys
      do j = 1, n_ints
        key(j) = i + j
      enddo
      call h%set(key, i * 0.5d0)
    enddo

    call cpu_time(t1)

    call h%clear()
    call cpu_time(t2)

    write(*,'(a,g0.3)') "Time to assemble: ", t1 - t0
    write(*,'(a,g0.3)') "Time to clear: ", t2 - t1
  end subroutine
end program
