program find_patterns
  implicit none
  integer, parameter :: n = 1000
  real :: random_numbers(n)
  integer :: i, j, length
  integer, allocatable :: pattern(:), sequence(:)
  logical :: pattern_found

  ! Generate random numbers
  call random_seed() ! Initialize random seed
  call random_number(random_numbers)

  ! Loop through possible pattern lengths and search for patterns
  do length = 2, n / 10
    pattern_found = .false.

    ! Loop through the array and search for repeating sequences
    do i = 1, n - length + 1
      allocate(pattern(length))
      pattern = [ (random_numbers(i + k - 1), k = 0, length - 1) ]

      do j = i + length, n - length + 1
        allocate(sequence(length))
        sequence = [ (random_numbers(j + k - 1), k = 0, length - 1) ]

        if (all(pattern == sequence)) then
          print *, "Pattern found starting at index:", i, " with length:", length
          print *, "Pattern values:", pattern
          pattern_found = .true.
          exit
        endif
        deallocate(sequence)
      end do
      deallocate(pattern)

      if (pattern_found) exit
    end do
    if (pattern_found) then
      exit
    end if
  end do

  if (.not. pattern_found) then
    print *, "No repeating pattern found"
  end if

end program find_patterns
