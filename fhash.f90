#ifdef __DO_NOT_PREPROCESS_DOC__
! Hash table implementation imitating to GCC STL (with singly linked list).
! DO NOT COMPILE THIS TEMPLATE FILE DIRECTLY.
! Use a wrapper module and include this file instead, e.g. fhash_modules.f90.
! Remove is not implemented since not needed currently.
!
! #define                         | meaning
! --------------------------------+-----------------------------------------------------
! SHORTNAME <Name>                | (optional) The name of the type this FHASH table is
!                                 | for. If set, it overrides all settings that have
!                                 | have possibly been made for FHASH_MODULE_NAME,
!                                 | FHASH_TYPE_NAME and FHASH_TYPE_ITERATOR_NAME.
!                                 |
! FHASH_MODULE_NAME <Name>        | The name of the module that encapsulates the FHASH
!                                 | types and functionality
! FHASH_TYPE_NAME <Name>          | The name of the actual FHASH type
! FHASH_TYPE_ITERATOR_NAME <Name> | The name of the FHASH type that can iterate through
!                                 | the whole FHASH
!                                 |
! KEY_USE <use stmt>              | (optional) A use statement that is required to use
!                                 | a specific type as a key for the FHASH
! KEY_TYPE <typename>             | The type of the keys. May require KEY_USE to be
!                                 | accessible.
! KEYS_EQUAL_FUNC <function>      | (optional) function that returns whether two keys
!                                 | are equal. Defaults to `==`.
!                                 |
! VALUE_USE <use stmt>            | (optional) A use statement that is required to use
!                                 | a specific type as a value for the FHASH
! VALUE_TYPE <typename>           | The type of the values. May require VALUE_USE to be
!                                 | accessible.
! 
! HASH_FUNC                   | (optional) hash function name. Defaults to 'hash'.
!                                 |
! VALUE_VALUE                     | Flag indicating that the values in FHASH are value
!                                 | values. This is the default. (see VALUE_POINTER)
! VALUE_POINTER                   | Flag indicating that the values in FHASH are value
!                                 | pointers.
! VALUE_ASSIGNMENT                | (internal) The assignment operator, do not set it
!                                 | anywhere, it is configured based on VALUE_VALUE or
!                                 | VALUE_POINTER
#endif

#ifdef SHORTNAME
#undef FHASH_MODULE_NAME
#undef FHASH_TYPE_NAME
#undef FHASH_TYPE_ITERATOR_NAME

#ifdef __GFORTRAN__
#define PASTE(a) a
#define CONCAT(a,b) PASTE(a)b
#else
#define PASTE(a,b) a ## b
#define CONCAT(a,b) PASTE(a,b)
#endif
#define FHASH_MODULE_NAME CONCAT(fhash_module__,SHORTNAME)
#define FHASH_TYPE_NAME CONCAT(fhash_type__,SHORTNAME)
#define FHASH_TYPE_ITERATOR_NAME CONCAT(fhash_type_iterator__,SHORTNAME)
#endif

#ifndef HASH_FUNC
#define HASH_FUNC hash_value
#endif
  
#undef VALUE_ASSIGNMENT
#ifndef VALUE_VALUE
#ifndef VALUE_POINTER
#define VALUE_VALUE
#endif
#endif

#ifdef VALUE_POINTER
#define VALUE_ASSIGNMENT =>
#else
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
      procedure, non_overridable :: node_set

      ! If kv is not allocated, fail and return 0.
      ! If key is present and the same as the key passed in, return the value in kv.
      ! If next pointer is associated, delegate to it.
      ! Otherwise, fail and return 0.
      procedure, non_overridable :: node_get

      ! If kv is not allocated, fail and return
      ! If key is present and node is first in bucket, set first node in bucket to
      !   the next node of first. Return success
      ! If key is present and the node is another member of the linked list, link the
      !   previous node's next node to this node's next node, deallocate this node,
      !   return success
      ! Otherwise, fail and return 0
      procedure, non_overridable :: node_remove
      
      ! Return the length of the linked list start from the current node.
      procedure, non_overridable :: node_depth
      
      ! Deallocate kv if allocated.
      ! Call the clear method of the next node if the next pointer associated.
      ! Deallocate and nullify the next pointer.
      !
      ! Need separate finalizers because a resursive procedure cannot be elemental.
      final :: clear_scalar_node
      final :: clear_rank1_nodes
  end type

  type FHASH_TYPE_NAME
    private

    integer :: n_buckets = 0
    integer :: n_keys = 0
    type(node_type), allocatable :: buckets(:)

    contains
      ! Returns the number of buckets.
      procedure, non_overridable, public :: bucket_count

      ! Return the number of collisions.
      procedure, non_overridable, public :: n_collisions

      ! Reserve certain number of buckets.
      procedure, non_overridable, public :: reserve

      ! Returns number of keys.
      procedure, non_overridable, public :: key_count

      ! Set the value at a given a key.
      procedure, non_overridable, public :: set

      ! Get the value at the given key.
      procedure, non_overridable, public :: get

      ! Remove the value with the given key.
      procedure, non_overridable, public :: remove

      ! Clear all the allocated memory
      procedure, non_overridable, public :: clear

      procedure, non_overridable, private :: key2bucket
  end type

  type FHASH_TYPE_ITERATOR_NAME
    private

    integer :: bucket_id
    type(node_type), pointer :: node_ptr => null()
    type(FHASH_TYPE_NAME), pointer :: fhash_ptr => null()

    contains
      ! Set the iterator to the beginning of a hash table.
      procedure, non_overridable, public :: begin

      ! Get the key value of the next element and advance the iterator.
      procedure, non_overridable, public :: next
  end type

  contains
  logical function keys_equal(a, b)
    KEY_TYPE, intent(in) :: a, b

#ifdef KEYS_EQUAL_FUNC
    keys_equal = KEYS_EQUAL_FUNC(a, b)
#else
    keys_equal = a == b
#endif
  end function

  function bucket_count(this)
    class(FHASH_TYPE_NAME), intent(in) :: this
    integer :: bucket_count

    bucket_count = this%n_buckets
  end function

  function n_collisions(this)
    class(FHASH_TYPE_NAME), intent(in) :: this
    integer :: n_collisions
    integer :: i

    n_collisions = 0
    do i = 1, this%n_buckets
      n_collisions = n_collisions + node_depth(this%buckets(i)) - 1
    enddo
  end function

  recursive function node_depth(this) result(depth)
    class(node_type), intent(in) :: this
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

    integer :: i
    integer, parameter :: sizes(*) = [5, 11, 23, 47, 97, 199, 409, 823, 1741, 3469, 6949, 14033, &
    & 28411, 57557, 116731, 236897, 480881, 976369,1982627, 4026031, &
    & 8175383, 16601593, 33712729, 68460391, 139022417, 282312799, &
    & 573292817, 1164186217, 2147483647]

    call assert(this%key_count() == 0, 'Cannot reserve when fhash is not empty.')
    call assert(sizes(size(sizes)) >= n_buckets, "Did not expect to need this many buckets.")

    do i = 1, size(sizes)
      if (sizes(i) >= n_buckets) then
        this%n_buckets = sizes(i)
        allocate(this%buckets(this%n_buckets))
        exit
      endif
    enddo
  end subroutine

  function key_count(this)
    class(FHASH_TYPE_NAME), intent(in) :: this
    integer :: key_count

    key_count = this%n_keys
  end function

  subroutine set(this, key, value)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, intent(in) :: value
    integer :: bucket_id
    logical :: is_new

    bucket_id = this%key2bucket(key)
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
    else if (keys_equal(this%kv%key, key)) then
      this%kv%value VALUE_ASSIGNMENT value
      if (present(is_new)) is_new = .false.
    else
      if (.not. associated(this%next)) allocate(this%next)
      call this%next%node_set(key, value, is_new)
    endif
  end subroutine

  subroutine get(this, key, value, success)
    class(FHASH_TYPE_NAME), intent(in) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, intent(out) :: value
    logical, optional, intent(out) :: success
    integer :: bucket_id

    bucket_id = this%key2bucket(key)
    call this%buckets(bucket_id)%node_get(key, value, success)
  end subroutine

  recursive subroutine node_get(this, key, value, success)
    class(node_type), intent(in) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, intent(out) :: value
    logical, optional, intent(out) :: success

    if (.not. allocated(this%kv)) then
      ! Not found. (Initial node in the bucket not set)
      if (present(success)) success = .false.
    else if (keys_equal(this%kv%key, key)) then
      value VALUE_ASSIGNMENT this%kv%value
      if (present(success)) success = .true.
    else if (associated(this%next)) then
      call this%next%node_get(key, value, success)
    else
      if (present(success)) success = .false.
    endif
  end subroutine
  
  subroutine remove(this, key, success)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    logical, optional, intent(out) :: success

    integer :: bucket_id
    logical ::  locSuccess
    
    bucket_id = this%key2bucket(key)
    associate(first => this%buckets(bucket_id))
      if (.not. allocated(first%kv)) then
        locSuccess = .false.
      elseif (.not. keys_equal(first%kv%key, key)) then
        call node_remove(first%next, key, locSuccess, first)
      elseif (associated(first%next)) then
        first%kv%key =  first%next%kv%key
        first%kv%value VALUE_ASSIGNMENT this%buckets(bucket_id)%next%kv%value
        deallocate(first%next%kv)
        first%next => first%next%next
        locSuccess = .true.
      else
        deallocate(first%kv)
        locSuccess = .true.
      endif
    end associate
    
    if (locSuccess) this%n_keys = this%n_keys - 1
    if (present(success)) success = locSuccess
  end subroutine

  recursive subroutine node_remove(this, key, success, last)
    class(node_type), intent(inout) :: this, last
    KEY_TYPE, intent(in) :: key
    logical, intent(out) :: success
    
    if (.not. allocated(this%kv)) then
      ! Not found. (Initial node in the bucket not set)
      success = .false.
    else if (keys_equal(this%kv%key, key)) then
      last%next => this%next
      nullify(this%next)
      deallocate(this%kv)
      success = .true.
    else if (associated(this%next)) then
      call this%next%node_remove(key, success, this)
    else
      success = .false.
    endif
  end subroutine

  subroutine clear(this)
    class(FHASH_TYPE_NAME), intent(out) :: this
  end subroutine
  
  integer function key2bucket(this, key) result(bucket_id)
    class(FHASH_TYPE_NAME), intent(in) :: this
    KEY_TYPE, intent(in) :: key

    bucket_id = modulo(HASH_FUNC(key), this%n_buckets) + 1
  end function


  subroutine clear_rank1_nodes(nodes)
    type(node_type), intent(inout) :: nodes(:)

    integer :: i

    do i = 1, size(nodes)
      call clear_scalar_node(nodes(i))
    enddo
  end subroutine

  recursive subroutine clear_scalar_node(node)
    type(node_type), intent(inout) :: node

    if (associated(node%next)) then
      call clear_scalar_node(node%next)
      deallocate(node%next)
    endif
  end subroutine

  subroutine begin(this, fhash_target)
    class(FHASH_TYPE_ITERATOR_NAME), intent(inout) :: this
    type(FHASH_TYPE_NAME), target, intent(in) :: fhash_target

    this%bucket_id = 1
    call assert(allocated(fhash_target%buckets), "cannot start iteration when fhash is empty")
    this%node_ptr => fhash_target%buckets(1)
    this%fhash_ptr => fhash_target
  end subroutine

  subroutine next(this, key, value, status)
    class(FHASH_TYPE_ITERATOR_NAME), intent(inout) :: this
    KEY_TYPE, intent(out) :: key
    VALUE_TYPE, intent(out) :: value
    integer, optional, intent(out) :: status

    do
      if (associated(this%node_ptr)) then
        if (allocated(this%node_ptr%kv)) exit
      endif

      if (this%bucket_id < this%fhash_ptr%n_buckets) then
        this%bucket_id = this%bucket_id + 1
        this%node_ptr => this%fhash_ptr%buckets(this%bucket_id)
      else
        if (present(status)) status = -1
#ifdef VALUE_TYPE_INIT
        value VALUE_ASSIGNMENT VALUE_TYPE_INIT
#endif
        return
      endif
    enddo

    key = this%node_ptr%kv%key
    value VALUE_ASSIGNMENT this%node_ptr%kv%value
    if (present(status)) status = 0
    this%node_ptr => this%node_ptr%next

  end subroutine

  subroutine assert(condition, msg)
    use, intrinsic :: iso_fortran_env, only: error_unit
    logical, intent(in) :: condition
    character(*), intent(in) :: msg

    if (.not. condition) then
      write(error_unit, '(a)') msg
      error stop
    endif
  end subroutine
end module

#undef KEY_TYPE
#undef KEYS_EQUAL_FUNC
#undef VALUE_TYPE
#undef VALUE_TYPE_INIT
#undef VALUE_ASSIGNMENT
#undef FHASH_TYPE_NAME
#undef HASH_FUNC
#undef FHASH_TYPE_ITERATOR_NAME
#undef SHORTNAME
#undef CONCAT
#undef PASTE
