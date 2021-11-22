#ifdef __DO_NOT_PREPROCESS_DOC__
! Hash table implementation imitating to GCC STL (with singly linked list).
! DO NOT COMPILE THIS TEMPLATE FILE DIRECTLY.
! Use a wrapper module and include this file instead, e.g. fhash_modules.f90.
! Remove is not implemented since not needed currently.
!
! #define                         | meaning
! --------------------------------+-----------------------------------------------------
! FHASH_NAME <Name>               | The name of the type of FHASH table.
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

#ifdef __GFORTRAN__
#    define PASTE(a) a
#    define CONCAT(a,b) PASTE(a)b
#else
#    define PASTE(a,b) a ## b
#    define CONCAT(a,b) PASTE(a,b)
#endif
#define FHASH_MODULE_NAME CONCAT(FHASH_NAME,_mod)
#define FHASH_TYPE_NAME CONCAT(FHASH_NAME,_t)
#define FHASH_TYPE_ITERATOR_NAME CONCAT(FHASH_NAME,_iter_t)
#define FHASH_TYPE_KV_TYPE_NAME CONCAT(FHASH_NAME,_kv_t)
#define FHASH_SORT_KV_NAME CONCAT(sort_,FHASH_NAME)

! For some bizar reason both gfortran-10 and ifort-2021.4 fail to compile, unless
! this function has a unique name for every time that this file is included:
#define __COMPARE_AT_IDX CONCAT(fhash_type_compare__,FHASH_NAME)

#ifdef VALUE_POINTER
#   define VALUE_ASSIGNMENT =>
#else
#   define VALUE_ASSIGNMENT =
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
  public :: FHASH_TYPE_KV_TYPE_NAME
  public :: FHASH_SORT_KV_NAME ! for convenience, because it's hard for the users to write a generic sort
                         ! (that circumvents the compiler bugs when passing pointers to internal functions to `qsort`)

  type :: FHASH_TYPE_KV_TYPE_NAME
    KEY_TYPE :: key
    VALUE_TYPE :: value
  end type

  type :: node_type
    type(FHASH_TYPE_KV_TYPE_NAME), allocatable :: kv
    type(node_type), pointer :: next => null()

    contains
      ! Return the length of the linked list start from the current node.
      procedure, non_overridable :: node_depth

      ! No FINAL procedure here, because it would have to be recursive (at least
      ! implicitly, because it finalizes the 'next' pointer), and a recursive
      ! procedure is not performant.
      ! Fortunately this type is not public, and it gets deallocated when finalizing the fhash.
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
      generic :: get_ptr => get_ptr_or_autoval, get_ptr_or_null
      procedure, non_overridable, public :: get_ptr_or_null
      procedure, non_overridable, public :: get_ptr_or_autoval
#endif

      ! Remove the value with the given key.
      procedure, non_overridable, public :: remove

      ! Get the key/value pairs as a list:
      procedure, non_overridable, public :: as_list
      procedure, non_overridable, public :: as_sorted_list

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

  interface
    integer function compare_keys_i(a, b)
      import
      implicit none
      KEY_TYPE, intent(in) :: a, b
    end function
  end interface
  procedure(compare_keys_i), pointer :: global_compare_ptr => null()
  type(FHASH_TYPE_KV_TYPE_NAME), pointer :: global_sorted_kv_list_ptr(:) => null()

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
    class(FHASH_TYPE_NAME), intent(out) :: this
    integer, intent(in) :: n_buckets

    integer :: i
    integer, parameter :: sizes(*) = [5, 11, 23, 47, 97, 199, 409, 823, 1741, 3469, 6949, 14033, &
    & 28411, 57557, 116731, 236897, 480881, 976369,1982627, 4026031, &
    & 8175383, 16601593, 33712729, 68460391, 139022417, 282312799, &
    & 573292817, 1164186217, 2147483647]
    integer, parameter :: n = size(sizes)

    call assert(sizes(2:) - sizes(:n-1) > 0, "PROGRAMMING ERROR: sizes should be strictly increasing")
    call assert(sizes(n) >= n_buckets, "Did not expect to need this many buckets.")

    do i = 1, n
      if (sizes(i) >= n_buckets) then
        allocate(this%buckets(sizes(i)))
        exit
      endif
    enddo
  end subroutine

  impure elemental function key_count(this)
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
    call node_set(this%buckets(bucket_id), key, value, is_new)

    if (is_new) this%n_keys = this%n_keys + 1
  end subroutine

  recursive subroutine node_set(this, key, value, is_new)
    ! If kv is not allocated, allocate and set to the key, value passed in.
    ! If key is present and the same as the key passed in, overwrite the value.
    ! Otherwise, defer to the next node (allocate if not allocated)
    type(node_type), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, intent(in) :: value
    logical, intent(out) :: is_new

    if (.not. allocated(this%kv)) then
      allocate(this%kv)
      this%kv%key = key
      this%kv%value VALUE_ASSIGNMENT value
      is_new = .true.
    else if (keys_equal(this%kv%key, key)) then
      this%kv%value VALUE_ASSIGNMENT value
      is_new = .false.
    else
      if (.not. associated(this%next)) allocate(this%next)
      call node_set(this%next, key, value, is_new)
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
    elseif (.not. associated(this%next)) then
      if (present(success)) success = .false.
    else
      call node_get(this%next, key, value, success)
    endif
  end subroutine
  
#ifndef VALUE_POINTER
  function get_ptr_or_null(this, key) result(value)
    class(FHASH_TYPE_NAME), intent(in) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, pointer :: value

    integer :: bucket_id
    type(node_type), pointer :: bucket

    call assert(associated(this%buckets), "get: fhash has not been initialized")
    
    bucket_id = this%key2bucket(key)
    call assert(1 <= bucket_id .and. bucket_id <= size(this%buckets), "get: fhash has not been initialized")
    bucket => this%buckets(bucket_id)

    value => node_get_ptr_or_null(bucket, key)
  end function

  recursive function node_get_ptr_or_null(this, key) result(value)
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
      value => node_get_ptr_or_null(this%next, key)
    endif
  end function

  function get_ptr_or_autoval(this, key, autoval) result(value)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, intent(in) :: autoval
    VALUE_TYPE, pointer :: value

    integer :: bucket_id
    type(node_type), pointer :: bucket
    logical :: is_new

    call assert(associated(this%buckets), "get: fhash has not been initialized")
    
    bucket_id = this%key2bucket(key)
    call assert(1 <= bucket_id .and. bucket_id <= size(this%buckets), "get: fhash has not been initialized")
    bucket => this%buckets(bucket_id)

    call node_get_ptr_or_autoval(bucket, key, value, is_new, autoval)
    if (is_new) this%n_keys = this%n_keys + 1
  end function

  recursive subroutine node_get_ptr_or_autoval(this, key, value, is_new, autoval)
    type(node_type), target, intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    VALUE_TYPE, pointer, intent(out) :: value
    logical, intent(out) :: is_new
    VALUE_TYPE, intent(in) :: autoval

    if (.not. allocated(this%kv)) then
      allocate(this%kv)
      this%kv%key = key
      this%kv%value = autoval
      value => this%kv%value
      is_new = .true.
    else if (keys_equal(this%kv%key, key)) then
      value => this%kv%value
      is_new = .false.
    else if (.not. associated(this%next)) then
      allocate(this%next)
      allocate(this%next%kv)
      this%next%kv%key = key
      this%next%kv%value = autoval
      value => this%next%kv%value
      is_new = .true.
    else
      call node_get_ptr_or_autoval(this%next, key, value, is_new, autoval)
    endif
  end subroutine
#endif

  subroutine remove(this, key, success)
    class(FHASH_TYPE_NAME), intent(inout) :: this
    KEY_TYPE, intent(in) :: key
    logical, optional, intent(out) :: success

    integer :: bucket_id
    logical ::  locSuccess
    type(node_type), pointer :: first, temp
    
    call assert(associated(this%buckets), "remove: fhash has not been initialized")

    bucket_id = this%key2bucket(key)
    first => this%buckets(bucket_id)

    if (.not. allocated(first%kv)) then
      locSuccess = .false.
    elseif (.not. keys_equal(first%kv%key, key)) then
      call node_remove(first, key, locSuccess)
    elseif (associated(first%next)) then
      call move_alloc(first%next%kv, first%kv)
      temp => first%next
      first%next => first%next%next
      deallocate(temp)
      locSuccess = .true.
    else
      deallocate(first%kv)
      locSuccess = .true.
    endif

    if (locSuccess) this%n_keys = this%n_keys - 1
    if (present(success)) success = locSuccess
  end subroutine

  recursive subroutine node_remove(last, key, success)
    ! If kv is not allocated, fail and return
    ! If key is present and node is first in bucket, set first node in bucket to
    !   the next node of first. Return success
    ! If key is present and the node is another member of the linked list, link the
    !   previous node's next node to this node's next node, deallocate this node,
    !   return success
    ! Otherwise, fail and return 0
    type(node_type), intent(inout) :: last
    KEY_TYPE, intent(in) :: key
    logical, intent(out) :: success
    
    type(node_type), pointer :: next

    next => last%next

    if (.not. allocated(next%kv)) then
      success = .false.
    else if (keys_equal(next%kv%key, key)) then
      last%next => next%next
      nullify(next%next)
      deallocate(next%kv)
      success = .true.
    else if (.not. associated(next%next)) then
      success = .false.
    else
      call node_remove(next, key, success)
    endif
  end subroutine

  subroutine as_list(this, kv_list)
    class(FHASH_TYPE_NAME), target, intent(in) :: this
    type(FHASH_TYPE_KV_TYPE_NAME), intent(out) :: kv_list(:)

    integer :: i, n
    type(FHASH_TYPE_ITERATOR_NAME) :: iter
    integer :: iter_stat
    
    n = this%key_count()
    call assert(size(kv_list) == n, "as_list: kv_list has a bad size")

    call iter%begin(this)
    do i = 1, n
      call iter%next(kv_list(i)%key, kv_list(i)%value, iter_stat)
      call assert(iter_stat == 0, "as_list: internal error: iterator stopped unexpectedly")
    enddo
  end subroutine

  subroutine as_sorted_list(this, kv_list, compare)
    class(FHASH_TYPE_NAME), target, intent(in) :: this
    type(FHASH_TYPE_KV_TYPE_NAME), target, intent(out) :: kv_list(:)
    procedure(compare_keys_i) :: compare

    call this%as_list(kv_list)
    call FHASH_SORT_KV_NAME(kv_list, compare)
  end subroutine

  subroutine FHASH_SORT_KV_NAME(kv_list, compare)
    type(FHASH_TYPE_KV_TYPE_NAME), target, intent(inout) :: kv_list(:)
    procedure(compare_keys_i) :: compare

    call assert(.not. (associated(global_compare_ptr) .or. associated(global_sorted_kv_list_ptr)), &
        "It looks like I am already sorting, and this is not thread-safe.")
    global_compare_ptr => compare
    global_sorted_kv_list_ptr => kv_list

    call permute(kv_list, sorting_perm())

    global_compare_ptr => null()
    global_sorted_kv_list_ptr => null()
  end subroutine

  subroutine permute(x, perm)
    ! Performs
    !     x = x(perm)
    ! but (i) this is more efficient, and (ii) ifort appears to put `x(perm)` on
    ! the stack before copying, causing a segfault for large arrays.
    use, intrinsic :: iso_c_binding, only: c_int
    use, intrinsic :: iso_fortran_env, only: int8, int16

    type(FHASH_TYPE_KV_TYPE_NAME), intent(inout) :: x(:)
    integer(c_int), intent(in) :: perm(:)

    type(FHASH_TYPE_KV_TYPE_NAME) :: temp
    integer :: i, n, j, jnew
    integer, parameter :: smallest_int = merge(int8, int16, int8 > 0)
    logical(smallest_int), allocatable :: done(:)

    call assert(size(x) ==  size(perm), "INTERNAL ERROR: permute: inconsistent sizes")
    n = size(x)

    allocate(done(n))
    done = .false._smallest_int
    do i = 1, n
      ! Follow the permutations, which form a cycle:
      j = i
      temp = x(i)
      do
        if (done(j)) exit
        jnew = perm(j)
        if (jnew == i) then
          x(j) = temp
        else
          x(j) = x(jnew)
        endif
        done(j) = .true._smallest_int
        j = jnew
      enddo
    enddo
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

    integer :: i

    this%n_keys = 0
    if (associated(this%buckets)) then
      do i = 1, size(this%buckets)
        call clear_children(this%buckets(i))
        if (allocated(this%buckets(i)%kv)) deallocate(this%buckets(i)%kv)
      enddo
      deallocate(this%buckets)
    endif
  end subroutine

#ifdef _FINAL_IS_IMPLEMENTED
  impure elemental subroutine clear_final(this)
    type(FHASH_TYPE_NAME), intent(inout) :: this

    call this%clear()
  end subroutine
#endif

  subroutine clear_children(node)
    ! Not a recursive subroutine, because (i) this is much more performant, and
    ! (ii) gfortran thinks that it cannot be both elemental and recursive.
    _FINAL_TYPEORCLASS(node_type), intent(inout) :: node

    type(node_type), pointer :: prev, next

    next => node%next
    do
      if (.not. associated(next)) return
      prev => next
      next => prev%next
      deallocate(prev)
    enddo
  end subroutine

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

  impure elemental subroutine assert(condition, msg)
    use, intrinsic :: iso_fortran_env, only: error_unit
    logical, intent(in) :: condition
    character(*), intent(in) :: msg

    if (.not. condition) then
      write(error_unit, '(a)') msg
      error stop
    endif
  end subroutine

  integer(c_int) function __COMPARE_AT_IDX(c_a, c_b) bind(C)
    use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

    type(c_ptr), value :: c_a, c_b

    integer(c_int), pointer  :: f_a, f_b

    call c_f_pointer(c_a, f_a)
    call c_f_pointer(c_b, f_b)
    __COMPARE_AT_IDX = int(global_compare_ptr(global_sorted_kv_list_ptr(f_a)%key, &
                                              global_sorted_kv_list_ptr(f_b)%key), kind=c_int)
  end function

  function sorting_perm() result(perm)
    use, intrinsic :: iso_c_binding

    integer(c_int), allocatable, target :: perm(:)

    integer(c_int) :: i, n
    type(c_funptr) :: fun
    interface
      subroutine c_qsort(array, elem_count, elem_size, compare) bind(C, name="qsort")
        ! The function pointer has the interface
        !     int(*compar)(const void *, const void *)
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), value :: array
        integer(c_size_t), value :: elem_count
        integer(c_size_t), value :: elem_size
        type(c_funptr), value :: compare
      end subroutine
    end interface

    call assert(associated(global_sorted_kv_list_ptr) .and. associated(global_compare_ptr), &
        "internal error: global sorting state has not been set yet")

    n = size(global_sorted_kv_list_ptr, kind=c_int)
    allocate(perm(n))
    do i = 1, n
      perm(i) = i
    enddo
    fun = c_funloc(__COMPARE_AT_IDX)
    if (n > 0_c_int) call c_qsort(c_loc(perm(1)), int(n, kind=c_size_t), c_sizeof(perm(1)), fun)
  end function
end module

#undef FHASH_NAME
#undef FHASH_MODULE_NAME
#undef FHASH_TYPE_NAME
#undef FHASH_TYPE_ITERATOR_NAME
#undef FHASH_TYPE_KV_TYPE_NAME
#undef HASH_FUNC
#undef _FINAL_IS_IMPLEMENTED
#undef _FINAL_TYPEORCLASS
#undef __COMPARE_AT_IDX
#undef KEY_TYPE
#undef KEYS_EQUAL_FUNC
#undef VALUE_TYPE
#undef VALUE_TYPE_INIT
#undef VALUE_ASSIGNMENT
#undef CONCAT
#undef PASTE
