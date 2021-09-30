! Define the module for the key type.
! Override the hash_value and == operator interface.
module ints_module

  implicit none

  type ints_type
    integer, allocatable :: ints(:)
  end type

  interface hash_value
    module procedure hash_value_ints
  end interface

  interface operator (==)
    module procedure ints_equal
  end interface

#ifdef __GFORTRAN__
  interface assignment (=)
    module procedure ints_ptr_assign
  end interface 
#endif
  
  contains

    function hash_value_ints(ints) result(hash)
      type(ints_type), intent(in) :: ints
      integer :: hash
      integer :: i

      hash = 0
      do i = 1, size(ints%ints)
        hash = ieor(hash, ints%ints(i) + 1640531527 + ishft(hash, 6) + ishft(hash, -2))
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

#ifdef __GFORTRAN__
    subroutine ints_ptr_assign(lhs, rhs)
      type(ints_type), pointer, intent(inout) :: lhs
      type(ints_type), pointer, intent(in) :: rhs
      lhs => rhs
    end subroutine
#endif

end module ints_module

! Define the macros needed by fhash and include fhash.f90
#define KEY_USE use ints_module
#define KEY_TYPE type(ints_type)
#define VALUE_USE use, intrinsic :: iso_fortran_env
#define VALUE_TYPE real(real64)
#define VALUE_TYPE_INIT 0.0
#define SHORTNAME ints_double
#include "fhash.f90"

module int_module
  implicit none

  interface hash_value
    module procedure hash_value_int
  end interface

  contains

    function hash_value_int(int) result(hash)
      integer, intent(in) :: int
      integer :: hash

      hash = int
    end function
end module

! Define the macros needed by fhash and include fhash.f90
#define KEY_USE use int_module
#define KEY_TYPE integer
#define VALUE_USE use ints_module
#define VALUE_TYPE type(ints_type), pointer
!#define VALUE_TYPE_INIT null()
#define SHORTNAME int_ints_ptr
#ifndef __GFORTRAN__
#define VALUE_POINTER
#endif
#ifdef VALUE_TYPE_INIT
#define CHECK_ITERATOR_VALUE
#endif
#include "fhash.f90"
