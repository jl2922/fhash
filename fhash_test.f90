program fhash_test

  use fhash_module

  call test_contructor()
  call test_reserve()

  print *, "ALL TESTS PASSED."

  contains

    subroutine test_contructor()
      type(fhash_type) h

      h = fhash_type()
      if (h%key_count() /= 0) stop 'expect no keys.'
    end subroutine

    subroutine test_reserve()
      type(fhash_type) h

      h = fhash_type()
      call h%reserve(3)
      if (h%bucket_count() < 3) stop 'expect at least 3 buckets'
    end subroutine


end program