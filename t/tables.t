# tables.t --- testing the many various ways to do tables in Pod6

use v6;
use Test;

use Grammar;
use Actions;
use World;

my $*W = Earth.new;

#?DOES 1
sub parse-ok($str, $msg) { lives-ok { Pod6::Grammar.parse($str, :actions(Pod6::Actions)) }, $msg }

#?DOES 1
sub parse-nok($str, $msg) { dies-ok { Pod6::Grammar.parse($str, :actions(Pod6::Actions)) }, $msg }

=begin comment

Since the current spec is far too vague about tables, here are the current rules
I've envisioned for how this works:

=item # Single-lined tables are those whose rows aren't separated by intervening
        lines. Each cell is only one line vertically.

=item # Multi-lined tables are those whose rows are separated by lines
        consisting of any of the following:

    =item2 # 'Blank' lines, those consisting of just horizontal whitespace, or
             are alternately empty.

    =item2 # The use of "ASCII" row separator characters (any of C<->, C<_>, or
             C<=>), or intersection characters (currently just C<+>) where
             column separators cross.

    =item2 # By use of the Unicode box drawing characters, using the appropriate
             characters between rows and at intersections.

        =item3 You may mix styles in a row, only by using the appropriate
               'hybrid' characters. This means (as of Unicode 8.0) that you must
               choose either the heavy/light box style or the single/double-line
               box style.

=item # Columns are separated by either of the following:

    =item2 # Two or more horizontal whitespace characters (tabs are not
             (currently) counted as multiple whitespace characters).

    =item2 # An "ASCII" column separator between cells (either C<|> or C<#>),
             and intersection characters where row separators cross the column
             separators.

    =item2 # The appropriate column/intersection box drawing characters.

    =item2 # For "ASCII" and box-drawing separators, the separators between
             cells must be surrounded on both sides by whitespace.

=item # You cannot mix the whitespace, "ASCII", and box-drawing styles in a
        table. By the end of the first non-whitespace line in the table, the
        parser has decided on one of those three modes. The parser looks for
        non-whitespace separators first, assuming whitespace only as a fallback.

=item # The whitespace style is meant primarily for simple tables; it only
        supports one header row at the top, and only if single-line style is
        used for the rest of the rows. Whitespace style can't have multi-row or
        multi-column cells, multiple header rows, or header columns.

=item # Certain characters are considered 'light' or 'heavy', which factors into
        header rows/columns:

    =item2 In whitespace mode, a blank row separator is the 'heavy' separator,
           while no separation (single-line mode) is the 'light'
           separator. Whitespace mode only has one column separator, so it
           doesn't matter for column separation.

    =item2 In "ASCII" mode, C<=> is the heavy row separator, and C<_> and C<->
           are both the light row separator. For columns, C<|> is light while
           C<#> is heavy.

    =item2 In Unicode mode, the heavy and double box-drawing characters are
           considered the heavy versions, while the light and single characters
           are the light versions.

        =item3 Hybrid characters are considered light characters where they
               matter.

    =item2 Intersection characters are always ignored for light/heavy
           considerations; they can be thought of as neutral.

    =item2 If a mixture of light and heavy characters occurs in a row or column
           separator, the separator is considered to be light. In other words,
           heavy separators only exists when the separator consists of just
           heavy characters.

=item # Header rows are marked with a heavy row separator between the header
        rows and the content rows. Header rows are only considered at the top of
        the table currently.

    =item2 If another heavy row separator occurs in the table, then the table is
           considered not to have any header rows.

    =item2 Multiple header rows must be internally separated by light row
           separators.

    =item2 All header rows are read as multi-line rows. Having multiple,
           single-line header rows is not possible.

    =item2 A light row separator may separate the header row only on single-line
           tables.

=item # After a header row separator, single-line is suspected until
        encountering a row separator (border-less tables), or a non-blank line
        after a row separator (bordered tables).

=item # Header columns are marked with a heavy column separator between the
        header columns and the content rows. Header columns are only considered
        at the left of the table currently.

    =item2 The occurrence of another heavy column separator means the table has
           no header columns.

    =item2 Multiple header columns are separated internally by light column
           separators.

=item # Tables not in whitespace style may have a border around them, detected
        if the first non-whitespace line of the table is a row separator. Such
        bordered tables must be bordered on all sides.

    =item2 The appropriate intersection characters must be used at the corners
           of the border, and where the row and column separators meet.

    =item2 Borders are not counted towards heavy/light separator considerations,
          so you could e.g. surround the whole table in a 'heavy' border without
          ruining row/column header detection (otherwise the bottom/right border
          would always turn off header interpretation if you did a heavy style,
          and the top/left border would be uselessly considered header
          separators when heavy).

=item # Table cells may span multiple rows and/or columns, so long as the cell
        is still a rectangle.

    =item2 Row and column separators must have all correctly-positioned
           intersection characters, even if the some of the cells it borders go
           over their respective lines. Those intersection points are used to
           detect spanning cells.

    =item2 Multi-column cells are detected by missing column separators, more
           accurately when the next column separator found has skipped one or
           more intersections.

    =item2 Multi-row cells are detected by partially missing row separators
           (where intersections would indicate there should be one).

    =item2 Multi-column I<and> multi-row cells will die if it finds that the
           cell is not rectangular. This is found by a partial row separator in
           the cell being read, on a line where a row separator would
           appear. (Partial column separators would currently not be caught,
           instead read as part of the cell text.)

    =item2 For multi-column I<and> -row cells, you don't need to (and shouldn't)
           put isolated intersection characters where they'd otherwise be.

The tests below better illustrate what I think of as valid and invalid.

NOTE: I should consider some sort of nice table converter, which takes an ASCII
style table as input and produces a nice Unicode table, for those who want to
have nice unicode table, just easily. (I'd also like an emacs table editor that
uses box-drawing chars, but alas...)

=end comment

# no borders, no headers, single lines (separation by _multiple_ whitespace)

parse-ok(q:to/END_TBL/, "Single-line headerless whitespace-style table parses");
=table
    Smith        John
    Adams        Douglas
    Wall         Larry
END_TBL

# no borders, whitespace-separated header, single line records

parse-ok(q:to/END_TBL/, "Header row, whitespace-style, single-line table parses");
=begin table
    Last Name    First Name

    Smith        John
    Adams        Douglas
    Wall         Larry
=end table
END_TBL

# 'ASCII' style, no border

parse-ok(q:to/END_TBL/, "Borderless ASCII style with header parses");
=for table
    Last Name | First Name
    ==========+===========
        Smith | John
    ----------+-----------
        Adams | Douglas
    ----------+-----------
         Wall | Larry
END_TBL

# 'ASCII' style, border, header column & header row, single line

parse-ok(q:to/END_TBL/, "Bordered single-line ASCII with header row & column parses");
=for table
+---+------------------+
| v # Released         |
+===+==================+
| 1 # 18 December 1987 |
| 2 # 5 June 1988      |
| 3 # 18 October 1989  |
| 4 # 21 March 1991    |
| 5 # 17 October 1994  |
| 6 # 25 December 2015 |
+---+------------------+
END_TBL

# light/heavy box drawing style, multi-line header row

parse-ok(q:to/END_TBL/, "Unicode light/heavy style, header row parses");
=for table
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

parse-ok(q:to/END_TBL/, "Bordered single/double Unicode, header rows & header column parses");
=for table
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

parse-ok(q:to/END_TBL/, "Unicode table, header rows & column parses");
=for table
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