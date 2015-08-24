# test.p6 --- temporary testing file for Pod parser

use v6;

use lib "lib/";
use Grammar;

# TEMP TEST

my $*FILENAME = "<internal-test>";

my $testpod = q:to/NOTPOD/;

    =end everything
    =begin pod :!autotoc
    =       :autotoc :imconfused :!ok

    =config L<> :okthen

    Hello there L<ALL OK >
    Everybody

    Another para

        AND SOME CODE C««
            GLORIOUS»» CODE
        HELLO SAILOR

    =encoding iso8859-1

    V<>=alias FOOBAR quuxy

    And one more para
        Hanging indent!!~~

    =end pod

        =end code

    =encoding aosdf aoish ao
    sdifao sodk
NOTPOD

Pod6::Grammar.parse($testpod);

for @<block> {
    say ~$_
}