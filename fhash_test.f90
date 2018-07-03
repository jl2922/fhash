program fhash_test

  use, intrinsic :: iso_fortran_env
  use fhash_module__ints_double
  use fhash_module__int_ints_ptr
  use ints_module

  implicit none

  real :: start, finish
  integer :: numKeys

  call test_contructor()
  call test_reserve()
  call test_insert_and_get_ints_double()
  call test_insert_and_get_int_ints_ptr()
  call test_insert_get_and_remove_int_ints_ptr()
  call test_iterate()

  print *, 'ALL TESTS PASSED.'
  print *, 'Start benchmark:'

  ! Benchmark
  numKeys = 10000000
#ifdef __GFORTRAN__
  if (__SIZEOF_POINTER__ == 8) numKeys = numKeys * 2
#else
  if (int_ptr_kind() == 8) numKeys = numKeys * 2
#endif
  call cpu_time(start)
  call benchmark(2, numKeys)
  call cpu_time(finish)
  print '("Time finish = ", G0.3," seconds.")', finish - start

  contains

  subroutine test_contructor()
    type(fhash_type__ints_double) h
    if (h%key_count() /= 0) stop 'expect no keys'
  end subroutine

  subroutine test_reserve()
    type(fhash_type__ints_double) h
    call h%reserve(3)
    if (h%bucket_count() /= 5) stop 'expect to reserve 5 buckets'
  end subroutine

  subroutine test_insert_and_get_ints_double()
    type(fhash_type__ints_double) :: h
    type(ints_type) :: key
    real(real64) :: value
    integer :: i
    logical :: success
    call h%reserve(5)
    allocate(key%ints(10))

    key%ints = 0
    do i = 1, 10
      key%ints(i) = i
      call h%get(key, value, success)
      if (success) stop 'expect not found'
      call h%set(key, i * 0.5_real64)
      call h%get(key, value)
      if (abs(value - i * 0.5_real64) > epsilon(value)) stop 'expect to get 0.5 i'
    enddo
    if (h%key_count() /= 10) stop 'expect key count to be 10'
    if (h%n_collisions() >= 10 .or. h%n_collisions() < 5) stop 'expect n_collisions in [5, 10)'

    call h%clear()
    if (h%key_count() /= 0) stop 'expect no keys'
    if (h%bucket_count() /= 0) stop 'expect no buckets'
  end subroutine

  subroutine test_insert_and_get_int_ints_ptr()
    type(fhash_type__int_ints_ptr) :: h
    type(ints_type), target :: value
    type(ints_type), pointer :: value_ptr, value_ptr2, value_ptr3
    logical :: success
    call h%reserve(5)
    allocate(value%ints(10))
    value%ints = 0
    value_ptr => value
    call h%set(0, value_ptr)
    call h%get(0, value_ptr2, success)
    if (value_ptr2%ints(1) /= 0) stop 'expect ints(1) to be 0'
    value_ptr2%ints(1) = 1

    call h%get(0, value_ptr3, success)
    if (value_ptr3%ints(1) /= 1) stop 'expect ints(1) to be 1'
  end subroutine

  subroutine test_insert_get_and_remove_int_ints_ptr()
    type(fhash_type__int_ints_ptr) :: h
    integer, parameter ::  num_values = 50
    type(ints_type), pointer :: pValues(:), pValue
    logical :: success
    integer ::  i, key, status
    type(fhash_type_iterator__int_ints_ptr) :: it
    
    ! prepare
    allocate(pValues(num_values))
    
    ! create
    call h%reserve(5)
    
    ! add
    do i = 1, num_values
      pValue => pValues(i)
      allocate(pValue%ints(2))
      pValue%ints(1) = i
      call h%set(i, pValue)
    end do
    
    if (h%key_count() .ne. num_values) stop 'expect different key count'

    ! get
    do i = num_values, i, -1
      nullify(pValue)
      call h%get(i, pValue, success)
      if (.not. success) stop 'expect a value for given key '
      if (pValue%ints(1) .ne. pValues(i)%ints(1)) stop 'expect different value for given key'
    end do
    
    ! remove first item
    do i = 1, num_values
      if (mod(i, 5) .eq. 1) then
        call h%remove(i, success)
        if (.not. success) stop 'expect to successfully remove item with given key '
      endif
    end do
    if (h%key_count() .ne. num_values-10) stop 'expect different key count'

    ! remove first item (fail)
    do i = 1, num_values
      if (mod(i, 5) .eq. 1) then
        call h%remove(i, success)
        if (success) stop 'expect that remove item with given key fails'
      endif
    end do
    if (h%key_count() .ne. num_values-10) stop 'expect  different key count'

    ! remove middle item
    do i = 1, num_values
      if (mod(i, 5) .eq. 4) then
        call h%remove(i, success)
        if (.not. success) stop 'expect to successfully remove item with given key '
      endif
    end do
    if (h%key_count() .ne. num_values-20) stop 'expect different key count'

    nullify (pValue)

    ! Setup iterator.
    call it%begin(h)
    do while (.true.)
      call it%next(key, pValue, status)
      if (status /= 0) exit
      if (key .ne. pValue%ints(1)) stop 'expect to retrieve matching key value pair'
      if (mod(key, 5) .eq. 1) stop 'expect not to get deleted keys'
      if (mod(key, 5) .eq. 4) stop 'expect not to get deleted keys'
    end do
    
    if (associated(pValue)) stop 'expect .not. associated(pValue)'
    
    deallocate(pValues)
    
  end subroutine  
  
  subroutine test_iterate()
    type(fhash_type__ints_double) :: h
    type(fhash_type_iterator__ints_double) :: it
    type(ints_type) :: key
    real(real64) :: value
    integer :: i, j
    integer :: status
    logical, allocatable :: found(:)
    integer :: i_found
    call h%reserve(10)
    allocate(key%ints(10))

    ! Setup keys and values.
    key%ints = 0
    do i = 1, 10
      key%ints(i) = i
      call h%set(key, i * 0.5_real64)
    enddo
    
    ! Setup iterator.
    call it%begin(h)

    allocate(found(10))
    found(:) = .false.
    
    do i = 1, 10
      call it%next(key, value, status)
      if (status /= 0) stop 'expect to get key value with status 0'

      ! Check for consistency.
      i_found = nint(value / 0.5)
      if (found(i_found)) stop 'expect no duplicate'
      found(i_found) = .true.
      do j = 1, i_found
        if (key%ints(j) /= j) stop 'expect to get j'
      enddo
      do j = i_found + 1, 10
        if (key%ints(j) /= 0) stop 'expect to get 0'
      enddo
    enddo

    ! Check end of hash table.
    call it%next(key, value, status)
    if (status /= -1) stop 'expect to return -1'
    call h%clear()
  end subroutine

  subroutine benchmark(n_ints, n_keys)
    integer, intent(in) :: n_ints, n_keys
    type(fhash_type__ints_double) :: h
    type(ints_type) :: key
    real :: start, finish
    integer :: i, j

    print '("n_ints: ", I0, ", n_keys: ", I0)', n_ints, n_keys

    call cpu_time(start)
    call h%reserve(n_keys * 2)
    allocate(key%ints(n_ints))
    do i = 1, n_keys
      do j = 1, n_ints
        key%ints(j) = i + j
      enddo
      call h%set(key, (i + j) * 0.5_real64)
    enddo
    call cpu_time(finish)
    print '("Time insert = ", G0.3," seconds.")', finish - start
    call h%clear()
  end subroutine

end program
