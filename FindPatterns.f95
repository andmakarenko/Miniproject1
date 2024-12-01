program time_loops_detection
    implicit none
    integer, parameter :: n = 1000, len = 5
    integer :: i, j, max_frequency
    real :: avg
    integer, allocatable :: random_nums(:)
    character(len=256), allocatable :: patterns(:)
    integer, allocatable :: frequencies(:)
    character(len=256) :: max_key

    ! Generate random list
    call generate_random_list(n, random_nums)

    ! Detect repeating patterns
    call find_time_loops(random_nums, n, len, patterns, frequencies)

    ! Look for outliers
    call look_for_outliers(patterns, frequencies)

contains

    subroutine generate_random_list(size, nums)
        integer, intent(in) :: size
        integer, allocatable, intent(out) :: nums(:)
        real, allocatable :: temp(:)
        integer :: i

        allocate(nums(size))
        allocate(temp(size))

        call random_seed()  ! Initialize random number generator
        call random_number(temp)  ! Generate random numbers in [0, 1)

        do i = 1, size
            nums(i) = int(temp(i) * 10)  ! Scale to [0, 9]
        end do

        deallocate(temp)
    end subroutine generate_random_list

    subroutine find_time_loops(nums, size, length, patterns, frequencies)
    integer, intent(in) :: nums(:), size, length
    character(len=256), allocatable, intent(out) :: patterns(:)
    integer, allocatable, intent(out) :: frequencies(:)
    integer :: i, j, k, pattern_count
    character(len=256) :: pattern
    logical :: found

    pattern_count = 0
    allocate(patterns(size - length + 1))
    allocate(frequencies(size - length + 1))
    frequencies = 0

    do i = 1, size - length + 1
        ! Create the pattern string
        pattern = ""
        do j = i, i + length - 1
            if (j > i) then
                pattern = trim(adjustl(pattern)) // ","
            end if
            pattern = trim(adjustl(pattern)) // trim(adjustl(itoa(nums(j))))
        end do

        ! Check if the pattern already exists
        found = .false.
        do k = 1, pattern_count
            if (patterns(k) == pattern) then
                frequencies(k) = frequencies(k) + 1
                found = .true.
                exit
            end if
        end do

        if (.not. found) then
            pattern_count = pattern_count + 1
            patterns(pattern_count) = pattern
            frequencies(pattern_count) = 1
        end if
    end do

    ! Filter out patterns with frequency less than 2
    call filter_patterns(patterns, frequencies, pattern_count)
end subroutine find_time_loops

    subroutine resize_arrays(patterns, frequencies, count)
        character(len=256), allocatable, intent(inout) :: patterns(:)
        integer, allocatable, intent(inout) :: frequencies(:)
        integer, intent(in) :: count
        patterns = patterns(1:count)
        frequencies = frequencies(1:count)
    end subroutine resize_arrays

    subroutine look_for_outliers(patterns, frequencies)
        character(len=256), allocatable, intent(in) :: patterns(:)
        integer, allocatable, intent(in) :: frequencies(:)
        integer :: i, max_frequency
        real :: avg
        character(len=256) :: max_key

        max_frequency = 0
        avg = 0.0
        max_key = ""

        ! Calculate average and find max frequency
        do i = 1, size(frequencies)
            avg = avg + frequencies(i)
            if (frequencies(i) > max_frequency) then
                max_frequency = frequencies(i)
                max_key = patterns(i)
            end if
        end do
        avg = avg / real(size(frequencies))

        ! Print results
        print *, "Gefundene Zeitschleifen:"
        do i = 1, size(frequencies)
            print *, "Muster: ", trim(patterns(i)), " | Wiederholungen: ", frequencies(i)
        end do

        if (max_frequency - avg > 3.0) then
            print *, "Vermeintliche Zeitschleife gefunden!"
            print *, "Muster: ", trim(max_key), " wiederholt ", max_frequency, " mal."
        else
            print *, "Keine vermeintliche Zeitschleife gefunden!"
        end if
    end subroutine look_for_outliers
    
    subroutine filter_patterns(patterns, frequencies, count)
    character(len=256), allocatable, intent(inout) :: patterns(:)
    integer, allocatable, intent(inout) :: frequencies(:)
    integer, intent(inout) :: count
    integer :: i, new_count
    character(len=256), allocatable :: new_patterns(:)
    integer, allocatable :: new_frequencies(:)

    ! Count valid patterns
    new_count = 0
    do i = 1, count
        if (frequencies(i) >= 2) then
            new_count = new_count + 1
        end if
    end do

    ! Allocate new arrays for valid patterns
    allocate(new_patterns(new_count))
    allocate(new_frequencies(new_count))

    ! Copy valid patterns to new arrays
    new_count = 0
    do i = 1, count
        if (frequencies(i) >= 2) then
            new_count = new_count + 1
            new_patterns(new_count) = patterns(i)
            new_frequencies(new_count) = frequencies(i)
        end if
    end do

    ! Replace original arrays with filtered arrays
    deallocate(patterns, frequencies)
    patterns = new_patterns
    frequencies = new_frequencies
    count = new_count
end subroutine filter_patterns

    function itoa(num) result(string)
        integer, intent(in) :: num
        character(len=32) :: string
        write(string, '(I0)') num
    end function itoa

end program time_loops_detection
