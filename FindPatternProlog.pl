:- use_module(library(random)).
:- use_module(library(lists)).

% Main entry point
main :-
    % Generate a list of 1000 random numbers between 1 and 9
    generate_random_list(1000, 1, 9, Numbers),

    % Calculate frequency of each number from 1 to 9
    calculate_frequency(1, 9, Numbers, Frequencies),
    
    % Print frequency of each number
    write('Frequency of each number:'), nl,
    print_frequencies(Frequencies, 1),

    % Find repeated sequences of length 3
    find_sequences_of_length_3(Numbers, Sequences),
    calculate_sequence_frequencies(Sequences, SequenceCounts),
    
    % Print sequences that appear more than 4 times
    write('\nRepeated sequences of length 3 that come up more than 4 times:'), nl,
    print_repeated_sequences(SequenceCounts).

% Generate a list of N random numbers between Min and Max
generate_random_list(0, _, _, []) :- !.
generate_random_list(N, Min, Max, [X | Xs]) :-
    N > 0,
    random_between(Min, Max, X),
    N1 is N - 1,
    generate_random_list(N1, Min, Max, Xs).

% Calculate the frequency of each number between Min and Max
calculate_frequency(Max, Max, Numbers, [Count]) :-
    count_occurrences(Max, Numbers, Count), !.
calculate_frequency(Min, Max, Numbers, [Count | Counts]) :-
    Min =< Max,
    count_occurrences(Min, Numbers, Count),
    Min1 is Min + 1,
    calculate_frequency(Min1, Max, Numbers, Counts).

% Count occurrences of an element in a list
count_occurrences(_, [], 0).
count_occurrences(Elem, [Elem | Tail], Count) :-
    count_occurrences(Elem, Tail, Count1),
    Count is Count1 + 1.
count_occurrences(Elem, [_ | Tail], Count) :-
    count_occurrences(Elem, Tail, Count).

% Print frequencies from Min to Max
print_frequencies([], _).
print_frequencies([Freq | Freqs], N) :-
    format('~d: ~d times~n', [N, Freq]),
    N1 is N + 1,
    print_frequencies(Freqs, N1).

% Find all sequences of length 3 in a list
find_sequences_of_length_3(List, Sequences) :-
    findall(Seq, (append(_, [A, B, C | _], List), Seq = [A, B, C]), Sequences).

% Calculate sequence frequencies
calculate_sequence_frequencies(Sequences, SequenceCounts) :-
    msort(Sequences, SortedSequences),
    encode_sequences(SortedSequences, SequenceCounts).

% Encode the frequency of each unique sequence
encode_sequences([], []).
encode_sequences([Seq | Rest], [Seq-Count | EncodedRest]) :-
    count_occurrences(Seq, [Seq | Rest], Count),
    delete(Rest, Seq, Remaining),
    encode_sequences(Remaining, EncodedRest).

% Print sequences repeated more than 4 times
print_repeated_sequences([]).
print_repeated_sequences([Seq-Count | Rest]) :-
    (   Count > 4
    ->  format('~w appears ~d times~n', [Seq, Count])
    ;   true
    ),
    print_repeated_sequences(Rest).

% Run the main program
:- main.
