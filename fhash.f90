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
!                                 | are equal. Defaults to `a == b` or `all(a == b)`,
!                                 | depending on whether the key is a scalar.
!                                 |
! VALUE_USE <use stmt>            | (optional) A use statement that is required to use
!                                 | a specific type as a value for the FHASH
! VALUE_TYPE <typename>           | The type of the values. May require VALUE_USE to be
!                                 | accessible.
! 
! HASH_FUNC                       | (optional) hash function name. Defaults to 'hash'.
!                                 |
! VALUE_POINTER                   | (optional) If defined, the values are pointers.
! VALUE_ASSIGNMENT                | (internal) The assignment operator, do not set it
!                                 | anywhere, it is configured based on VALUE_POINTER
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

#ifdef VALUE_POINTER
#define VALUE_ASSIGNMENT =>
#else
#define VALUE_ASSIGNMENT =
#endif

! Not all compilers implement finalization:
#if defined __GFORTRAN__ && __GNUC__ <= 5
#else
#   define _FINAL_IS_IMPLEMENTED
#endif
#ifdef _FINAL_IS_IMPLEMENTED
#   define _FINAL_TYPEORCLASS type
#else
#   define _FINAL_TYPEORCLASS class
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
#ifdef _FINAL_IS_IMPLEMENTED
      final :: clear_scalar_node
      final :: clear_rank1_nodes
#else
      ! Old `gfortran` versions think the passed dummy must be a scalar:
      generic, public :: clear => clear_scalar_node
      procedure, non_overridable, private :: clear_scalar_node
      ! procedure, non_overridable, private :: clear_rank1_nodes
#endif
    end type

  type FHASH_TYPE_NAME
    private

    integer :: n_keys = 0
    type(node_type), contiguous, pointer :: buckets(:) => null()

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

#ifndef VALUE_POINTER
      procedure, non_overridable, public :: get_ptr
#endif

      ! Remove the value with the given key.
      procedure, non_overridable, public :: remove

      ! Return the accumalated storage size of an fhash, including the underlying pointers.
      ! Takes the bit size of a key-value pair as an argument.
      procedure, non_overridable, public :: deep_storage_size => fhash_deep_storage_size

      ! Clear all the allocated memory
      procedure, non_overridable, public :: clear
#ifdef _FINAL_IS_IMPLEMENTED
      final :: clear_final
#endif
      generic, public :: assignment(=) => deepcopy_fhash
      procedure, non_overridable, private :: deepcopy_fhash

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

  interface default_hash
    module procedure :: default_hash__int
    module procedure :: default_hash__int_array
  end interface

  interface all
    module procedure :: scalar_all
  end interface

  contains
  logical function keys_equal(a, b)
    KEY_TYPE, intent(in) :: a, b

#ifdef KEYS_EQUAL_FUNC
    keys_equal = KEYS_EQUAL_FUNC(a, b)
#else
    keys_equal = all(a == b)
#endif
  end function

  function bucket_count(this)
    class(FHASH_TYPE_NAME), intent(in) :: this
    integer :: bucket_count

    if (.not. associated(this%buckets)) then
      bucket_count = 0
    else
      bucket_count = size(this%buckets)
    endif
  end function

  function n_collisions(this)
    class(FHASH_TYPE_NAME), intent(in) :: this
    integer :: n_collisions
    integer :: i

    call assert(associated(this%buckets), "n_collisions: fhash has not been initialized")

    n_collisions = 0
    do i = 1, size(this%buckets)
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

  impure elemental subroutine reserve(this, n_buckets)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    integer, intent(in) :: n_buckets

    integer :: i
    integer, parameter :: sizes(*) = [5, 11, 23, 47, 97, 199, 409, 823, 1741, 3469, 6949, 14033, &
    & 28411, 57557, 116731, 236897, 480881, 976369,1982627, 4026031, &
    & 8175383, 16601593, 33712729, 68460391, 139022417, 282312799, &
    & 573292817, 1164186217, 2147483647]

    call assert(this%key_count() == 0, 'Cannot reserve when fhash is not empty.')
    call assert(n_buckets >= 1, "I need at least one bucket.")
    call assert(sizes(size(sizes)) >= n_buckets, "Did not expect to need this many buckets.")

    do i = 1, size(sizes)
      if (sizes(i) >= n_buckets) then
        allocate(this%buckets(sizes(i)))
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

    call assert(associated(this%buckets), "set: fhash has not been initialized")

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

    call assert(associated(this%buckets), "get: fhash has not been initialized")

    bucket_id = this%key2bucket(key)
    call node_get(this%buckets(bucket_id), key, value, success)
  end subroutine

  recursive subroutine node_get(this, key, value, success)
    ! If kv is not allocated, fail and return 0.
    ! If key is present and the same as the key passed in, return the value in kv.
    ! If next pointer is associated, delegate to it.
    ! Otherwise, fail and return 0.
    type(node_type), intent(in) :: this
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
      call node_get(this%next, key, value, success)
    elseif (present(success)) then
      success = .false.
    endif
  end subroutine
  
#ifndef VALUE_POINTER
  function get_ptr(this, key) result(value)
    class(FHASH_TYPE_NAME), intent(in) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, pointer :: value

    integer :: bucket_id
    type(node_type), pointer :: bucket

    call assert(associated(this%buckets), "get: fhash has not been initialized")
    
    bucket_id = this%key2bucket(key)
    call assert(1 <= bucket_id .and. bucket_id <= size(this%buckets), "get: fhash has not been initialized")
    bucket => this%buckets(bucket_id)

    value => node_get_ptr(bucket, key)
  end function

  recursive function node_get_ptr(this, key) result(value)
    type(node_type), target, intent(in) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, pointer :: value

    if (.not. allocated(this%kv)) then
      value => null()
    else if (keys_equal(this%kv%key, key)) then
      value => this%kv%value
    else if (.not. associated(this%next)) then
      value => null()
    else
      value => node_get_ptr(this%next, key)
    endif
  end function
#endif

  subroutine remove(this, key, success)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    logical, optional, intent(out) :: success

    integer :: bucket_id
    logical ::  locSuccess
    
    call assert(associated(this%buckets), "remove: fhash has not been initialized")

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

  impure elemental subroutine deepcopy_fhash(lhs, rhs)
    class(FHASH_TYPE_NAME), intent(out) :: lhs
    type(FHASH_TYPE_NAME), intent(in) :: rhs

    integer :: i

    if (.not. associated(rhs%buckets)) return

    lhs%n_keys = rhs%n_keys
    allocate(lhs%buckets(size(rhs%buckets)))
    do i = 1, size(lhs%buckets)
      call deepcopy_node(rhs%buckets(i), lhs%buckets(i))
    enddo
  end subroutine

  recursive subroutine deepcopy_node(this, copy)
    class(node_type), intent(in) :: this
    type(node_type), intent(out) :: copy

    if (.not. allocated(this%kv)) then
      call assert(.not. associated(this%next), 'internal error: node has a "next" pointer, but it''s kv pair has not been set')
    else
      allocate(copy%kv, source=this%kv)
    endif

    if (associated(this%next)) then
      allocate(copy%next)
      call deepcopy_node(this%next, copy%next)
    endif
  end subroutine

  impure elemental integer function fhash_deep_storage_size(this, keyval_ss) result(s)
    class(FHASH_TYPE_NAME), intent(in) :: this
    integer, intent(in) :: keyval_ss

    integer :: i

    s = storage_size(this)
    if (associated(this%buckets)) then
      do i = 1, size(this%buckets)
        s = s + node_deep_storage_size(this%buckets(i), keyval_ss)
      enddo
    endif
  end function

  recursive integer function node_deep_storage_size(node, keyval_ss) result(s)
    type(node_type), intent(in) :: node
    integer, intent(in) :: keyval_ss

    s = storage_size(node) + keyval_ss
    if (associated(node%next)) s = s + node_deep_storage_size(node%next, keyval_ss)
  end function

  impure elemental subroutine clear(this)
    class(FHASH_TYPE_NAME), intent(inout) :: this

    this%n_keys = 0
    if (associated(this%buckets)) deallocate(this%buckets)
  end subroutine

#ifdef _FINAL_IS_IMPLEMENTED
  impure elemental subroutine clear_final(this)
    type(FHASH_TYPE_NAME), intent(inout) :: this

    call this%clear()
  end subroutine
#endif

  integer function key2bucket(this, key) result(bucket_id)
    class(FHASH_TYPE_NAME), intent(in) :: this
    KEY_TYPE, intent(in) :: key

    integer :: hash

#ifdef HASH_FUNC
    hash = HASH_FUNC(key)
#else
    hash = default_hash(key)
#endif
    bucket_id = modulo(hash, size(this%buckets)) + 1
  end function


  subroutine clear_rank1_nodes(nodes)
    _FINAL_TYPEORCLASS(node_type), intent(inout) :: nodes(:)

    integer :: i

    do i = 1, size(nodes)
      call clear_scalar_node(nodes(i))
    enddo
  end subroutine

  recursive subroutine clear_scalar_node(node)
    _FINAL_TYPEORCLASS(node_type), intent(inout) :: node

    if (associated(node%next)) then
      call clear_scalar_node(node%next)
      deallocate(node%next)
    endif
  end subroutine

  subroutine begin(this, fhash_target)
    class(FHASH_TYPE_ITERATOR_NAME), intent(inout) :: this
    type(FHASH_TYPE_NAME), target, intent(in) :: fhash_target

    call assert(associated(fhash_target%buckets), "cannot start iteration when fhash is empty")

    this%bucket_id = 1
    this%node_ptr => fhash_target%buckets(1)
    this%fhash_ptr => fhash_target
  end subroutine

  subroutine next(this, key, value, status)
    class(FHASH_TYPE_ITERATOR_NAME), intent(inout) :: this
    KEY_TYPE, intent(out) :: key
    VALUE_TYPE, intent(out) :: value
    integer, optional, intent(out) :: status

    call assert(associated(this%fhash_ptr), "next: iterator has not been initialized")
    
    do
      if (associated(this%node_ptr)) then
        if (allocated(this%node_ptr%kv)) exit
      endif

      if (this%bucket_id < size(this%fhash_ptr%buckets)) then
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

  integer function default_hash__int(key) result(hash)
    integer, intent(in) :: key

    hash = key
  end function

  integer function default_hash__int_array(key) result(hash)
    integer, intent(in) :: key(:)

    real(kind(1.0d0)), parameter :: phi = (sqrt(5.0d0) + 1) / 2
    ! Do not use `nint` intrinsic, because ifort claims that  "Fortran 2018 specifies that
    ! "an elemental intrinsic function here be of type integer or character and
    !  each argument must be an initialization expr of type integer or character":
    integer, parameter :: magic_number = 0.5d0 + 2.0d0**bit_size(hash) * (1 - 1 / phi)
    integer :: i

    hash = 0
    do i = 1, size(key)
      ! This triggers an error in `gfortran` (version 9.3.0) with the `-ftrapv` option.
      ! Compiler bug?
      hash = ieor(hash, key(i) + magic_number + ishft(hash, 6) + ishft(hash, -2))
    enddo
  end function

  logical function scalar_all(scal)
    logical, intent(in) :: scal

    scalar_all = scal
  end function

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

#undef _FINAL_IS_IMPLEMENTED
#undef _FINAL_TYPEORCLASS
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