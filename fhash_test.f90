program fhash_test

  use, intrinsic :: iso_fortran_env
  use fhash_module__ints_double
  use ints_module

  call test_contructor()
  call test_reserve()
  call test_insert_and_get()

  print *, "ALL TESTS PASSED."

  contains

    subroutine test_contructor()
      type(fhash_type__ints_double) h
      if (h%key_count() /= 0) stop 'expect no keys.'
    end subroutine

    subroutine test_reserve()
      type(fhash_type__ints_double) h
      call h%reserve(3)
      if (h%bucket_count() == 5) stop 'expect to reserve 5 buckets'
    end subroutine

    subroutine test_insert_and_get()
      type(fhash_type__ints_double) :: h
      type(ints_type) :: key
      integer :: i
      call h%reserve(5)
      allocate(key%ints(5))
      key%ints = 0
      do i = 1, 5
        key%ints(i) = i
        call h%insert(key, i * 0.5_real64)
      enddo
      if (h%key_count() /= 5) stop 'expect key count to be 5'
      
      key%ints = 0
      do i = 1, 5
        key%ints(i) = i
        if (abs(h%get(key) - i * 0.5_real64) > epsilon(0.0_real64)) stop 'expect to get 1.2'
      enddo
    end subroutine


end program