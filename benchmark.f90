#define KEY_ARRAY_SIZE 2

#define FHASH_NAME int2real
#define KEY_TYPE integer, dimension(KEY_ARRAY_SIZE)
#define VALUE_TYPE real(real64)
#define VALUE_USE use, intrinsic :: iso_fortran_env, only: real64
#include "fhash.f90"

program test_benchmark
  implicit none
  
  call benchmark(n_ints=KEY_ARRAY_SIZE, n_keys=10**7)
  
contains
  subroutine benchmark(n_ints, n_keys)
    use int2real_mod
    use iso_fortran_env, only: real64

    integer, intent(in) :: n_ints, n_keys

    type(int2real_t) :: h
    integer :: key(n_ints)
    integer :: i, j
    real :: t0, t1, t2, t3
    real(real64), pointer :: val

    write(*,'(a)') "Start fhash benchmark:"

    write(*,'("n_ints: ", I0, ", n_keys: ", I0)') n_ints, n_keys

    call h%reserve(n_keys * 2)
    call cpu_time(t0)

    do i = 1, n_keys
      do j = 1, n_ints
        key(j) = i + j
      enddo
      call h%set(key, i * 0.5d0)
    enddo
    call cpu_time(t1)

    do i = 1, n_keys
      do j = 1, n_ints
        key(j) = i + j
      enddo
      val => h%get_ptr(key) ! , autoval=3.0_real64)
    enddo
    call cpu_time(t2)

    call h%clear()
    call cpu_time(t3)

    write(*,'(a,3(g15.3))') "Time to assemble/ get / clear: ", t1 - t0, t2 - t1, t3 - t2
  end subroutine
end program
