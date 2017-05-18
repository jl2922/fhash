module fhash_module__/**/SHORTNAME

#ifdef KEY_USE
  KEY_USE
#endif
#ifdef VALUE_USE
  VALUE_USE
#endif

  implicit none

  private

  public :: fhash_type__/**/SHORTNAME

  type fhash_type__/**/SHORTNAME
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

      ! Insert a new key value pair.
      procedure, public :: insert

      ! Get the value at the given key.
      procedure, public :: get
  end type

  interface fhash_type__/**/SHORTNAME
    module procedure new_fhash
  end interface

  contains

  function new_fhash() result(this)
    type(fhash_type__/**/SHORTNAME) this

    this%n_buckets = 0
    this%n_keys = 0
  end function

  function bucket_count(this)
    class(fhash_type__/**/SHORTNAME), intent(inout) :: this
    integer :: bucket_count

    bucket_count = this%n_buckets
  end function

  subroutine reserve(this, n_buckets)
    class(fhash_type__/**/SHORTNAME), intent(inout) :: this
    integer, intent(in) :: n_buckets

    if (this%key_count() > 0) stop 'Cannot reserve when fhash is not empty.'
    this%n_buckets = n_buckets
  end subroutine

  function key_count(this)
    class(fhash_type__/**/SHORTNAME), intent(inout) :: this
    integer :: key_count

    key_count = this%n_keys
  end function

  subroutine insert(this, key, value)
    class(fhash_type__/**/SHORTNAME), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, intent(in) :: value
  end subroutine

  function get(this, key, success) result(value)
    class(fhash_type__/**/SHORTNAME), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    logical, intent(out) :: success
    VALUE_TYPE :: value

    value = 0
  end function

end module