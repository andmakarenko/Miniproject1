program random_frequency_sequence
  implicit none
  integer, parameter :: n = 1000
  integer :: numbers(n)
  integer :: i, j, num
  integer :: frequency(9)
  integer :: count, limit
  character(len=8) :: sequence
  character(len=8), dimension(:), allocatable :: sequences
  integer, dimension(:), allocatable :: sequence_counts
  logical :: found

  ! Initialize random seed
  call random_seed()

  ! Generate an array of 1000 random integers between 1 and 9
  call random_number_array(numbers, 1, 9)

  ! Initialize frequency array
  frequency = 0

  ! Find the frequency of each number
  do i = 1, n
     num = numbers(i)
     frequency(num) = frequency(num) + 1
  end do

  ! Print the frequency of each number
  print *, "Frequency of each number:"
  do i = 1, 9
     print *, i, ":", frequency(i), "times"
  end do

  ! Find repeated sequences of length 3
  allocate(sequences(n))
  allocate(sequence_counts(n))
  sequence_counts = 0
  count = 0

  do i = 1, n - 2
     write(sequence, '(I1, A, I1, A, I1)') numbers(i), ',', numbers(i + 1), ',', numbers(i + 2)

     found = .false.
     do j = 1, count
        if (trim(sequences(j)) == trim(sequence)) then
           sequence_counts(j) = sequence_counts(j) + 1
           found = .true.
           exit
        end if
     end do

     if (.not. found) then
        count = count + 1
        sequences(count) = sequence
        sequence_counts(count) = 1
     end if
  end do

  ! Print sequences repeated more than 4 times
  print *, "\nRepeated sequences of length 3 that come up more than 4 times:"
  do i = 1, count
     if (sequence_counts(i) > 4) then
        print *, trim(sequences(i)), "appears", sequence_counts(i), "times"
     end if
  end do

  ! Deallocate dynamic arrays
  deallocate(sequences)
  deallocate(sequence_counts)

contains

  ! Subroutine to generate an array of random integers between min_val and max_val
  subroutine random_number_array(array, min_val, max_val)
    implicit none
    integer, intent(out) :: array(:)
    integer, intent(in) :: min_val, max_val
    real :: random_val
    integer :: i
    do i = 1, size(array)
       call random_number(random_val)
       array(i) = min_val + int(random_val * (max_val - min_val + 1))
    end do
  end subroutine random_number_array

end program random_frequency_sequence
