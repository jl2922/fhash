program fhash_test

  use, intrinsic :: iso_fortran_env
  use fhash_module__ints_double
  use ints_module

  implicit none

  real :: start, finish

  call test_contructor()
  call test_reserve()
  call test_insert_and_get()

  print *, 'ALL TESTS PASSED.'

  ! Benchmark
  call cpu_time(start)
  call benchmark(14, 10000000)
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
      if (h%bucket_count() == 5) stop 'expect to reserve 5 buckets'
    end subroutine

    subroutine test_insert_and_get()
      type(fhash_type__ints_double) :: h
      type(ints_type) :: key
      real(real64) :: value
      integer :: i
      call h%reserve(10)
      allocate(key%ints(10))

      ! Set five different keys.
      key%ints = 0
      do i = 1, 10
        key%ints(i) = i
        call h%set(key, i * 0.5_real64)
      enddo
      if (h%key_count() /= 10) stop 'expect key count to be 10'
      
      ! Retrieve the five keys.
      key%ints = 0
      do i = 1, 10
        key%ints(i) = i
        call h%get(key, value)
        if (abs(value - i * 0.5_real64) > epsilon(value)) stop 'expect to get 1.2'
      enddo

      call h%clear()
      if (h%key_count() /= 0) stop 'expect no keys'
      if (h%bucket_count() /= 0) stop 'expect no buckets'
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