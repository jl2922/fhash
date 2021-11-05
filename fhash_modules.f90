#define SHORTNAME i2char
#define KEY_TYPE integer
#define VALUE_TYPE character(10)
#include "fhash.f90"

! Define the module for the key type.
module ints_module

  implicit none

  type ints_type
    integer, allocatable :: ints(:)
  end type

  interface hash_value
    module procedure hash_value_ints
  end interface

contains

  function hash_value_ints(ints) result(hash)
    use, intrinsic :: iso_fortran_env, only: int64, real64
    type(ints_type), intent(in) :: ints
    integer(kind(ints%ints)) :: hash

    real(real64), parameter :: phi = (sqrt(5.0_real64) + 1) / 2
    ! Do not use `nint` intrinsic, because ifort claims that  "Fortran 2018 specifies that
    ! "an elemental intrinsic function here be of type integer or character and
    !  each argument must be an initialization expr of type integer or character":
    integer, parameter :: magic_number = 0.5d0 + 2.0d0**bit_size(hash) * (1 - 1 / phi)
    integer :: i

    hash = 0
    do i = 1, size(ints%ints)
      ! This triggers an error in `gfortran` (version 9.3.0) with the `-ftrapv` option.
      ! Compiler bug?
      hash = ieor(hash, ints%ints(i) + magic_number + ishft(hash, 6) + ishft(hash, -2))
    enddo
  end function

  function ints_equal(lhs, rhs)
    type(ints_type), intent(in) :: lhs, rhs
    logical :: ints_equal
    integer :: i

    if (size(lhs%ints) /= size(rhs%ints)) then
      ints_equal = .false.
      return
    endif

    do i = 1, size(lhs%ints)
      if (lhs%ints(i) /= rhs%ints(i)) then
        ints_equal = .false.
        return
      endif
    enddo

    ints_equal = .true.

  end function
end module ints_module

! Define the macros needed by fhash and include fhash.f90
#define SHORTNAME ints_double
#define KEY_USE use ints_module
#define KEY_TYPE type(ints_type)
#define KEYS_EQUAL_FUNC ints_equal
#define VALUE_USE use, intrinsic :: iso_fortran_env
#define VALUE_TYPE real(real64)
#define HASH_FUNC hash_value
#define VALUE_TYPE_INIT 0.0
#include "fhash.f90"

! Define the macros needed by fhash and include fhash.f90
#define SHORTNAME int_ints_ptr
#define KEY_TYPE integer
#define VALUE_USE use ints_module
#define VALUE_TYPE type(ints_type), pointer
!#define VALUE_TYPE_INIT null()
#define VALUE_POINTER
#ifdef VALUE_TYPE_INIT
#define CHECK_ITERATOR_VALUE
#endif
#include "fhash.f90"
