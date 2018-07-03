! Hash table implementation imitating to GCC STL (with singly linked list).
! DO NOT COMPILE THIS TEMPLATE FILE DIRECTLY.
! Use a wrapper module and include this file instead, e.g. fhash_modules.f90.
! Remove is not implemented since not needed currently.

#ifndef VALUE_ASSIGNMENT
#define VALUE_ASSIGNMENT =
#endif
  
module FHASH_MODULE_NAME
#undef FHASH_MODULE_NAME

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

  public :: FHASH_TYPE_NAME
  public :: FHASH_TYPE_ITERATOR_NAME

  type kv_type
    KEY_TYPE :: key
    VALUE_TYPE :: value
  end type

  type node_type
    type(kv_type), allocatable :: kv
    type(node_type), pointer :: next => null()

    contains
      ! If kv is not allocated, allocate and set to the key, value passed in.
      ! If key is present and the same as the key passed in, overwrite the value.
      ! Otherwise, defer to the next node (allocate if not allocated)
      procedure :: node_set

      ! If kv is not allocated, fail and return 0.
      ! If key is present and the same as the key passed in, return the value in kv.
      ! If next pointer is associated, delegate to it.
      ! Otherwise, fail and return 0.
      procedure :: node_get

      ! Deallocate kv is allocated.
      ! Call the clear method of the next node if the next pointer associated.
      ! Deallocate and nullify the next pointer.
      procedure :: node_clear

      ! Return the length of the linked list start from the current node.
      procedure :: node_depth
  end type

  type FHASH_TYPE_NAME
    private

    integer :: n_buckets = 0
    integer :: n_keys = 0
    type(node_type), allocatable :: buckets(:)

    contains
      ! Returns the number of buckets.
      procedure, public :: bucket_count

      ! Return the number of collisions.
      procedure, public :: n_collisions

      ! Reserve certain number of buckets.
      procedure, public :: reserve

      ! Returns number of keys.
      procedure, public :: key_count

      ! Set the value at a given a key.
      procedure, public :: set

      ! Get the value at the given key.
      procedure, public :: get

      ! Clear all the allocated memory (must be called to prevent memory leak).
      procedure, public :: clear
  end type

  type FHASH_TYPE_ITERATOR_NAME
    private

    integer :: bucket_id
    type(node_type), pointer :: node_ptr => null()
    type(FHASH_TYPE_NAME), pointer :: fhash_ptr => null()

    contains
      ! Set the iterator to the beginning of a hash table.
      procedure, public :: begin

      ! Get the key value of the next element and advance the iterator.
      procedure, public :: next
  end type

  contains

  function bucket_count(this)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    integer :: bucket_count

    bucket_count = this%n_buckets
  end function

  function n_collisions(this)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    integer :: n_collisions
    integer :: i

    n_collisions = 0
    do i = 1, this%n_buckets
      n_collisions = n_collisions + node_depth(this%buckets(i)) - 1
    enddo
  end function

  recursive function node_depth(this) result(depth)
    class(node_type), intent(inout) :: this
    integer :: depth

    if (.not. associated(this%next)) then
      depth = 1
    else
      depth = 1 + node_depth(this%next)
    endif
  end function

  subroutine reserve(this, n_buckets)
    class(FHASH_TYPE_NAME), intent(inout) :: this
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
        this%n_buckets = sizes(i)
        allocate(this%buckets(this%n_buckets))
        return
      endif
    enddo
  end subroutine

  function key_count(this)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    integer :: key_count

    key_count = this%n_keys
  end function

  subroutine set(this, key, value)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, intent(in) :: value
    integer :: bucket_id
    logical :: is_new

    bucket_id = modulo(hash_value(key), this%n_buckets) + 1

    call this%buckets(bucket_id)%node_set(key, value, is_new)

    if (is_new) this%n_keys = this%n_keys + 1
  end subroutine

  recursive subroutine node_set(this, key, value, is_new)
    class(node_type), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, intent(in) :: value
    logical, optional, intent(out) :: is_new

    if (.not. allocated(this%kv)) then
      allocate(this%kv)
      this%kv%key = key
      this%kv%value VALUE_ASSIGNMENT value
      if (present(is_new)) is_new = .true.
    else if (this%kv%key == key) then
      this%kv%value VALUE_ASSIGNMENT value
      if (present(is_new)) is_new = .false.
    else
      if (.not. associated(this%next)) allocate(this%next)
      call this%next%node_set(key, value, is_new)
    endif
  end subroutine

  subroutine get(this, key, value, success)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, intent(out) :: value
    logical, optional, intent(out) :: success
    integer :: bucket_id

    bucket_id = modulo(hash_value(key), this%n_buckets) + 1
    call this%buckets(bucket_id)%node_get(key, value, success)
  end subroutine

  recursive subroutine node_get(this, key, value, success)
    class(node_type), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, intent(out) :: value
    logical, optional, intent(out) :: success

    if (.not. allocated(this%kv)) then
      ! Not found. (Initial node in the bucket not set)
      if (present(success)) success = .false.
    else if (this%kv%key == key) then
      value VALUE_ASSIGNMENT this%kv%value
      if (present(success)) success = .true.
    else if (associated(this%next)) then
      call this%next%node_get(key, value, success)
    else
      if (present(success)) success = .false.
    endif
  end subroutine

  subroutine clear(this)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    integer :: i

    if (.not. allocated(this%buckets)) return

    do i = 1, size(this%buckets)
      if (associated(this%buckets(i)%next)) then 
        call this%buckets(i)%next%node_clear()
        deallocate(this%buckets(i)%next)
      endif
    enddo
    deallocate(this%buckets)
    this%n_keys = 0
    this%n_buckets = 0
  end subroutine

  recursive subroutine node_clear(this)
    class(node_type), intent(inout) :: this

    if (associated(this%next)) then
      call this%next%node_clear()
      deallocate(this%next)
      nullify(this%next)
    endif
  end subroutine

  subroutine begin(this, fhash_target)
    class(FHASH_TYPE_ITERATOR_NAME), intent(inout) :: this
    type(FHASH_TYPE_NAME), target, intent(in) :: fhash_target

    this%bucket_id = 1 
    this%node_ptr => fhash_target%buckets(1)
    this%fhash_ptr => fhash_target
  end subroutine

  subroutine next(this, key, value, status)
    class(FHASH_TYPE_ITERATOR_NAME), intent(inout) :: this
    KEY_TYPE, intent(out) :: key
    VALUE_TYPE, intent(out) :: value
    integer, optional, intent(out) :: status

    do while (.not. associated(this%node_ptr) .or. .not. allocated(this%node_ptr%kv))
      if (this%bucket_id < this%fhash_ptr%n_buckets) then
        this%bucket_id = this%bucket_id + 1
        this%node_ptr => this%fhash_ptr%buckets(this%bucket_id)
      else
        if (present(status)) status = -1
        return
      endif
    enddo

    key = this%node_ptr%kv%key
    value VALUE_ASSIGNMENT this%node_ptr%kv%value
    if (present(status)) status = 0
    this%node_ptr => this%node_ptr%next

  end subroutine

end module

#undef KEY_TYPE
#undef VALUE_TYPE
#undef FHASH_TYPE_NAME
#undef FHASH_TYPE_ITERATOR_NAME
#undef VALUE_ASSIGNMENT