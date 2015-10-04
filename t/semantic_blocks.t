# semantic-blocks.t --- tests the semantic blocks

use v6;
use Test;

use Grammar;
use Actions;

plan 396;

# since the semantic blocks all behave the same, it's easy to test
# programmatically.

#?DOES 3
sub test-block($n) {
    my $ast = Pod6::Grammar.parse(qq:to/END_BLOCK/, :actions(Pod6::Actions)).ast;
        =begin $n
        This is a test of a semantic block.   This should be seen as an implicit
        para.

            And this is supposed to be a code block.   Hope it is!
        =end $n
        END_BLOCK

    isa-ok $ast, Pod6::Document, "$n block returns a Pod6::Document ast";

    isa-ok $ast[1], Pod6::Block::SEMANTIC, "$n block comes as Pod6::Block::SEMANTIC";
    is $ast[1].name, $n, "$n block has right name";

    isa-ok $ast[1][0], Pod6::Config, "$n block gets initial configuration block";
    isa-ok $ast[1][1], Pod6::Block::Para, "$n block implied =para";
    isa-ok $ast[1][2], Pod6::Block::Code, "$n block implied =code";
}

<NAME            NAMES
 AUTHOR          AUTHORS
 VERSION         VERSIONS
 CREATED         CREATEDS
 EMULATES        EMULATESES
 EXCLUDES        EXCLUDESES
 SYNOPSIS        SYNOPSES
 DESCRIPTION     DESCRIPTIONS
 USAGE           USAGES
 INTERFACE       INTERFACES
 METHOD          METHODS
 SUBROUTINE      SUBROUTINES
 OPTION          OPTIONS
 DIAGNOSTIC      DIAGNOSTICS
 ERROR           ERRORS
 WARNING         WARNINGS
 DEPENDENCY      DEPENDENCIES
 BUG             BUGS
 SEE-ALSO        SEE-ALSOS
 ACKNOWLEDGMENT  ACKNOWLEDGMENTS
 ACKNOWLEDGEMENT ACKNOWLEDGEMENTS
 COPYRIGHT       COPYRIGHTS
 DISCLAIMER      DISCLAIMERS
 LICENCE         LICENCES
 LICENSE         LICENSES
 TITLE           TITLES
 SECTION         SECTIONS
 CHAPTER         CHAPTERS
 APPENDIX        APPENDICES
 TOC             TOCS
 INDEX           INDICES
 FOREWORD        FOREWORDS
 SUMMARY         SUMMARIES>.map: { test-block($_) };