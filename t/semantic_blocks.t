# semantic-blocks.t --- tests the semantic blocks

use v6;
use Test;

use Grammar;
use Actions;
use World;

my $*W = Earth.new;

plan 330;

# since the semantic blocks all behave the same, it's easy to test
# programmatically.

#?DOES 3
sub test-block($n) {
    my $main = $n.key;
    my @names = $main, |$n.value;

    for @names {
        my $ast = Pod6::Grammar.parse(qq:to/END_BLOCK/, :actions(Pod6::Actions)).ast;
            =begin $_
            This is a test of a semantic block.   This should be seen as an
            implicit para.

                And this is supposed to be a code block.   Hope it is!
            =end $_
            END_BLOCK

            isa-ok $ast, Pod6::Document, "$_ block returns a Pod6::Document ast";

            isa-ok $ast[0], Pod6::Block::SEMANTIC, "$_ block comes as Pod6::Block::SEMANTIC";
            is $ast[0].name, $main, "$_ block has right name";

            isa-ok $ast[0][0], Pod6::Block::Para, "$_ block implied =para";
            isa-ok $ast[0][1], Pod6::Block::Code, "$_ block implied =code";
    }
}

{NAME           => "NAMES",
 AUTHOR         => "AUTHORS",
 VERSION        => "VERSIONS",
 CREATED        => "CREATEDS",
 EMULATES       => "EMULATESES",
 EXCLUDES       => "EXCLUDESES",
 SYNOPSIS       => "SYNOPSES",
 DESCRIPTION    => "DESCRIPTIONS",
 USAGE          => "USAGES",
 INTERFACE      => "INTERFACES",
 METHOD         => "METHODS",
 SUBROUTINE     => "SUBROUTINES",
 OPTION         => "OPTIONS",
 DIAGNOSTIC     => "DIAGNOSTICS",
 ERROR          => "ERRORS",
 WARNING        => "WARNINGS",
 DEPENDENCY     => "DEPENDENCIES",
 BUG            => "BUGS",
 SEE-ALSO       => "SEE-ALSOS",
 ACKNOWLEDGMENT => ("ACKNOWLEDGMENTS", "ACKNOWLEDGEMENT", "ACKNOWLEDGEMENTS"),
 COPYRIGHT      => "COPYRIGHTS",
 DISCLAIMER     => "DISCLAIMERS",
 LICENSE        => ("LICENSES", "LICENCE", "LICENCES"),
 TITLE          => "TITLES",
 SECTION        => "SECTIONS",
 CHAPTER        => "CHAPTERS",
 APPENDIX       => "APPENDICES",
 TOC            => "TOCS",
 INDEX          => "INDICES",
 FOREWORD       => "FOREWORDS",
 SUMMARY        => "SUMMARIES"}.map: { test-block($_) };