use strict;
use warnings;
use List::Util qw(shuffle);

# Generate an array of 1000 random integers between 1 and 9
my @numbers;
for my $i (1..1000) {
    push @numbers, int(rand(9)) + 1;
}

# Calculate frequency of each number
my %frequency;
foreach my $num (@numbers) {
    $frequency{$num}++;
}

# Print frequency of each number
print "Frequency of each number:\n";
foreach my $num (sort { $a <=> $b } keys %frequency) {
    print "$num: $frequency{$num} times\n";
}

# Find repeated sequences of length 3
my %sequence_count;
for my $i (0..$#numbers - 2) {
    my $sequence = join(",", @numbers[$i, $i + 1, $i + 2]);
    $sequence_count{$sequence}++;
}

# Print sequences repeated more than 4 times
print "\nRepeated sequences of length 3 that come up more than 4 times:\n";
foreach my $sequence (keys %sequence_count) {
    if ($sequence_count{$sequence} > 4) {
        print "$sequence appears $sequence_count{$sequence} times\n";
    }
}
