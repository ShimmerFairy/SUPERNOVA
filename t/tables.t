# tables.t --- testing the many various ways to do tables in Pod6

use v6;
use Test;

use Grammar;
use Actions;
use World;

my $*W = Earth.new;

#?DOES 1
sub parse-ok($str, $msg) { ok Pod6::Grammar.parse($str, :actions(Pod6::Actions)), $msg }

#?DOES 1
sub parse-nok($str, $msg) { dies-ok { Pod6::Grammar.parse($str, :actions(Pod6::Actions)) }, $msg }

#?DOES 3
sub test-cycle(&expect_delim, &expect_para, &expect_abbr, $str, $msg) {
    &expect_delim(qq:to/END_DELIM/, "$msg (=begin table)");
        =begin table
        $str
        =end table
        END_DELIM
    &expect_para(qq:to/END_PARA/, "$msg (=for table)");
        =for table
        $str
        END_PARA
    &expect_abbr(qq:to/END_ABBR/, "$msg (=table)");
        =table
        $str
        END_ABBR
}

# no borders, no headers, single lines (separation by _multiple_ whitespace)

todo("Style NYI", 3);
#test-cycle(&parse-ok, &parse-ok, &parse-ok,
#           q:to/END_TBL/, "Single-line headerless whitespace-style table parses");
#    Smith        John
#    Adams        Douglas
#    Wall         Larry
#END_TBL


# no borders, whitespace-separated header, single line records

todo("Style NYI", 3);
#test-cycle(&parse-ok, &parse-nok, &parse-nok,
#           q:to/END_TBL/, "Header row, whitespace-style, single-line table parses");
#    Last Name    First Name
#
#    Smith        John
#    Adams        Douglas
#    Wall         Larry
#END_TBL

# 'ASCII' style, no border
todo("Style NYI", 3);
#test-cycle(&parse-ok, &parse-ok, &parse-ok,
#           q:to/END_TBL/, "Borderless ASCII style with header parses");
#    Last Name | First Name
#    ==========+===========
#        Smith | John
#    ----------+-----------
#        Adams | Douglas
#    ----------+-----------
#         Wall | Larry
#END_TBL

# 'ASCII' style, border, header column & header row, single line
todo("Style NYI", 3);
#test-cycle(&parse-ok, &parse-ok, &parse-ok,
#           q:to/END_TBL/, "Bordered single-line ASCII with header row & column parses");
#+---+------------------+
#| v # Released         |
#+===+==================+
#| 1 # 18 December 1987 |
#| 2 # 5 June 1988      |
#| 3 # 18 October 1989  |
#| 4 # 21 March 1991    |
#| 5 # 17 October 1994  |
#| 6 # 25 December 2015 |
#+---+------------------+
#END_TBL

# light/heavy box drawing style, multi-line header row

test-cycle(&parse-ok, &parse-ok, &parse-ok,
           q:to/END_TBL/, "Unicode light/heavy style, header row parses");
┏━━━┳━━━━━━━━━━━━━━━━━━┓
┃ v ┃ Initial version  ┃
┃   ┃ release date     ┃
┡━━━╇━━━━━━━━━━━━━━━━━━┩
│ 1 │ 18 December 1987 │
├───┼──────────────────┤
│ 2 │ 5 June 1988      │
├───┼──────────────────┤
│ 3 │ 18 October 1989  │
├───┼──────────────────┤
│ 4 │ 21 March 1991    │
├───┼──────────────────┤
│ 5 │ 17 October 1994  │
├───┼──────────────────┤
│ 6 │ 25 December 2015 │
└───┴──────────────────┘
END_TBL

# single/double box drawing style, multiple header rows, header column, and
# spans

test-cycle(&parse-ok, &parse-ok, &parse-ok,
           q:to/END_TBL/, "Bordered single/double Unicode, header rows & header column parses");
╔═══╦════════╤═════════════╗
║   ║     Information      ║
╟   ╫────────┼─────────────╢
║ v ║ Latest │ First       ║
║   ║ Ver.   │ Release     ║
╠═══╬════════╪═════════════╣
║ 1 ║ 1.0_16 │ 18 Dec 1987 ║
╟───╫────────┼─────────────╢
║ 2 ║ 2.001  │ 05 Jun 1988 ║
╟───╫────────┼─────────────╢
║ 3 ║ 3.044  │ 18 Oct 1989 ║
╟───╫────────┼─────────────╢
║ 4 ║ 4.036  │ 21 Mar 1991 ║
╟───╫────────┼─────────────╢
║ 5 ║ 5.22.0 │ 17 Oct 1994 ║
╟───╫────────┼─────────────╢
║ 6 ║ 6.0.0  │ 25 Dec 2015 ║
╚═══╩════════╧═════════════╝
END_TBL

# oddball Unicode styling: heavy header rows, light table contents. Top border
# is light, curved edges. Last row separator uses dashed lines. Using "hybrid"
# straight characters to transition light top corners to heavy.

# note that the heavy/light style set has more characters for a mixture of light
# and heavy, compared to the single/double set, so there's more flexibility in
# design. The big thing is that you can line columns in a row header (or rows in
# a column header) with heavy characters, so long as those characters don't
# extend the entire way and end up making a second header separator. You can't
# really do that in single/double style

test-cycle(&parse-ok, &parse-ok, &parse-ok,
           q:to/END_TBL/, "Unicode table, header rows & column parses");
╭───┰────────┰─────────────╮
╽  Perl History Overview   ╽
┠───╂────────╂─────────────┨
┃ v ┃ Latest ┃ First       ┃
┃   ┃ Ver.   ┃ Release     ┃
┣━━━╋━━━━━━━━╇━━━━━━━━━━━━━┫
┃ 1 ┃ 1.0_16 │ 18 Dec 1987 ┃
┣━━━╉────────┼─────────────┨
┃ 2 ┃ 2.001  │ 05 Jun 1988 ┃
┣━━━╉────────┼─────────────┨
┃ 3 ┃ 3.044  │ 18 Oct 1989 ┃
┣━━━╉────────┼─────────────┨
┃ 4 ┃ 4.036  │ 21 Mar 1991 ┃
┣━━━╉────────┼─────────────┨
┃ 5 ┃ 5.22.0 │ 17 Oct 1994 ┃
┣┅┅┅╉┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┄┄┄┨
┃ 6 ┃ 6.0.0  │ 25 Dec 2015 ┃
┗━━━┻━━━━━━━━┷━━━━━━━━━━━━━┛
END_TBL

# XXX always an infinite number of variations left to test