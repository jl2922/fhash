module ints_module

  implicit none

  type ints_type
    integer, allocatable :: ints(:)
    contains
      procedure, public :: get_hash
  end type

  interface hash_value
    module procedure hash_value_ints
  end interface

  contains

    function get_hash(this) result(hash)
      class(ints_type), intent(in) :: this
      integer :: hash
      integer :: i

      hash = 0
      do i = 1, size(this%ints)
        hash = xor(hash, this%ints(i) + 1640531527 + ishft(hash, 6) + ishft(hash, -2))
      enddo
    end function

    function hash_value_ints(ints) result(hash)
      type(ints_type), intent(in) :: ints
      integer :: hash

      hash = ints%get_hash()
    end function

end module ints_module

#define KEY_TYPE type(ints_type)
#define VALUE_TYPE real(real64)
#define KEY_USE use ints_module
#define VALUE_USE use, intrinsic :: iso_fortran_env
#define SHORTNAME ints_double
#include "fhash.f90"