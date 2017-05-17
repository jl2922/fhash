module fhash_module

  implicit none

  private

  public :: fhash_type

  type fhash_type
    private

    integer :: n_buckets
    integer :: n_keys

    contains
      ! Returns the number of buckets.
      procedure, public :: bucket_count

      ! Reserve certain number of buckets.
      procedure, public :: reserve

      ! Returns number of keys.
      procedure, public :: key_count
  end type

  interface fhash_type
    module procedure new_fhash
  end interface

  contains

  function new_fhash() result(this)
    type(fhash_type) this

    this%n_buckets = 0
    this%n_keys = 0
  end function

  function bucket_count(this)
    class(fhash_type), intent(inout) :: this
    integer :: bucket_count

    bucket_count = this%n_buckets
  end function

  subroutine reserve(this, n_buckets)
    class(fhash_type), intent(inout) :: this
    integer, intent(in) :: n_buckets

    if (this%key_count() > 0) stop 'Cannot reserve when fhash is not empty.'
    this%n_buckets = n_buckets
  end subroutine

  function key_count(this)
    class(fhash_type), intent(inout) :: this
    integer :: key_count

    key_count = this%n_keys
  end function


end module