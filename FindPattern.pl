use strict;
use warnings;
use List::Util qw(shuffle);

# Generate a list of random numbers
sub generate_random_list {
    my ($n) = @_;
    my @random_nums;
    for (1..$n) {
        push @random_nums, int(rand(10));
    }
    return \@random_nums;
}

# Find repeating patterns in the list
sub find_time_loops {
    my ($nums, $len) = @_;
    my %pattern_frequency;

    for my $i (0..$#$nums - $len + 1) {
        my $pattern = join(',', @$nums[$i..$i + $len - 1]) . ',';
        $pattern_frequency{$pattern}++;
    }

    # Keep only patterns that occur more than once
    foreach my $key (keys %pattern_frequency) {
        delete $pattern_frequency{$key} if $pattern_frequency{$key} < 2;
    }

    return \%pattern_frequency;
}

# Look for outliers in the repeating patterns
sub look_for_outliers {
    my ($time_loops) = @_;
    my $avg = 0.0;
    my $max = 0;
    my $max_key = '';

    print "\nGefundene Zeitschleifen in timeLoops:\n";
    if (!%$time_loops) {
        print "Keine Zeitschleifen gefunden.\n";
    } else {
        foreach my $key (keys %$time_loops) {
            print "Muster: $key | Wiederholungen: $time_loops->{$key}\n";
            $avg += $time_loops->{$key};
            if ($time_loops->{$key} > $max) {
                $max = $time_loops->{$key};
                $max_key = $key;
            }
        }
    }

    $avg = $avg / (keys %$time_loops) if (keys %$time_loops);

    if ($max - $avg > 3) {
        print "\nVermeintliche Zeitschleife gefunden!\n";
        print "$max_key wurde $max mal wiederholt.\n";
    } else {
        print "\nKeine vermeintliche Zeitschleife gefunden!\n";
    }
}

# Main execution
my $random_nums = generate_random_list(1000);
my $len = 5;
my $random_time_loops = find_time_loops($random_nums, $len);
look_for_outliers($random_time_loops);
