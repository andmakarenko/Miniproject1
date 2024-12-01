:- use_module(library(random)).

% generate_random_list/2 generates a list of random integers between 0 and 9.
generate_random_list(0, []).
generate_random_list(N, [R|Rest]) :-
    N > 0,
    random_between(0, 9, R),
    N1 is N - 1,
    generate_random_list(N1, Rest).

% find_time_loops/3 finds repeated patterns of given length and returns their frequency.
find_time_loops(List, Len, TimeLoops) :-
    findall(Pattern, sublist_of_length(List, Len, Pattern), Patterns),
    count_patterns(Patterns, PatternCounts),
    include(repeated_pattern, PatternCounts, TimeLoops),
    print_repeated_patterns(TimeLoops).

% sublist_of_length/3 gets a sublist of given length from the original list.
sublist_of_length(List, Len, Sublist) :-
    append(_, Rest, List),
    append(Sublist, _, Rest),
    length(Sublist, Len).

% count_patterns/2 counts the occurrences of each pattern.
count_patterns(Patterns, PatternCounts) :-
    findall(Count-Pattern, (member(Pattern, Patterns), count_occurrences(Patterns, Pattern, Count)), PatternCountsUnsorted),
    sort(PatternCountsUnsorted, PatternCounts).

% count_occurrences/3 counts how many times a pattern occurs in the list.
count_occurrences(List, Element, Count) :-
    aggregate_all(count, member(Element, List), Count).

% repeated_pattern/1 filters patterns that occur more than once.
repeated_pattern(Count-_) :-
    Count > 1.

% print_repeated_patterns/1 prints the patterns that occur more than once.
print_repeated_patterns(TimeLoops) :-
    format('\nGefundene wiederholte Muster:\n'),
    forall(member(Count-Pattern, TimeLoops),
           format('Muster: ~w | Wiederholungen: ~d\n', [Pattern, Count])).

% look_for_outliers/1 identifies outliers in the time loops by looking at frequency differences.
look_for_outliers(TimeLoops) :-
    maplist(arg(1), TimeLoops, Counts),
    sum_list(Counts, Sum),
    length(Counts, Length),
    ( Length > 0 -> Avg is Sum / Length ; Avg = 0 ),
    max_member(Max, Counts),
    nth1(Index, Counts, Max),
    nth1(Index, TimeLoops, Max-Pattern),
    ( Max - Avg > 3 ->
        format('\nVermeintliche Zeitschleife gefunden!\n'),
        format('~w wurde ~d mal wiederholt.\n', [Pattern, Max])
    ;
        format('\nKeine vermeintliche Zeitschleife gefunden!\n')
    ).

% main/0 serves as the entry point for the program.
main :-
    % 1. Generate a random list of 1000 elements
    generate_random_list(1000, RandomNums),

    % 2. Find repeating patterns of length 5
    Len = 5,
    find_time_loops(RandomNums, Len, TimeLoops),

    % 3. Look for outliers in the time loops
    look_for_outliers(TimeLoops).

% To run the program, simply call main.
:- main.
