module fhash_module__/**/SHORTNAME

#ifdef KEY_USE
  KEY_USE
#undef KEY_USE
#endif
#ifdef VALUE_USE
  VALUE_USE
#undef VALUE_USE
#endif

  implicit none

  private

  public :: fhash_type__/**/SHORTNAME

  type kv_type
    KEY_TYPE :: key
    VALUE_TYPE :: value
  end type

  type nodes_type
    type(kv_type), allocatable :: kv
    type(nodes_type), pointer :: next => null()
  end type

  type fhash_type__/**/SHORTNAME
    private

    integer :: n_buckets = 0
    integer :: n_keys = 0
    type(nodes_type), allocatable :: buckets(:)

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

  contains

  function bucket_count(this)
    class(fhash_type__/**/SHORTNAME), intent(inout) :: this
    integer :: bucket_count

    bucket_count = this%n_buckets
  end function

  subroutine reserve(this, n_buckets)
    class(fhash_type__/**/SHORTNAME), intent(inout) :: this
    integer, intent(in) :: n_buckets
    integer, dimension(29) :: sizes
    integer :: i

    if (this%key_count() > 0) stop 'Cannot reserve when fhash is not empty.'

    sizes = (/5, 11, 23, 47, 97, 199, 409, 823, 1741, 3469, 6949, 14033, &
      & 28411, 57557, 116731, 236897, 480881, 976369,1982627, 4026031, &
      & 8175383, 16601593, 33712729, 68460391, 139022417, 282312799, &
      & 573292817, 1164186217, 2147483647/)
    do i = 1, size(sizes)
      if (sizes(i) >= n_buckets) then
        this%n_buckets = n_buckets
        allocate(this%buckets(this%n_buckets))
        exit
      endif
    enddo
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
    integer :: bucket_id

    bucket_id = modulo(hash_value(key), this%n_buckets)
    allocate(this%buckets(bucket_id)%kv)
    this%buckets(bucket_id)%kv%key = key
    this%buckets(bucket_id)%kv%value = value
    this%n_keys = this%n_keys + 1
  end subroutine

  function get(this, key, success) result(value)
    class(fhash_type__/**/SHORTNAME), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    logical, optional, intent(out) :: success
    VALUE_TYPE :: value
    integer :: bucket_id

    bucket_id = modulo(hash_value(key), this%n_buckets)
    if (.not. allocated(this%buckets(bucket_id)%kv)) then
      if (present(success)) success = .false.
      value = 0
      return
    endif

    value = this%buckets(bucket_id)%kv%value
    if (present(success)) success = .true.
  end function

end module

#undef KEY_TYPE
#undef VALUE_TYPE
#undef SHORTNAME