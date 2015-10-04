# block_types.t --- Test parsing of the standard block types

=begin comment

Note that V<=table> isn't test here, that deserves its own test file.

Semantic blocks are all in a separate test file, since they're somewhat
different from the standard blocks, and there are so many.

Blocks that imply para and/or code don't have the contents of those implied
blocks tested, since =para and =code blocks are already tested on their
own. Testing implied blocks shouldn't be necessary, but if needed those tests
can be added.

=end comment

use v6;
use Test;

#?DOES 1
sub isn't(|c) { isnt(|c) }

#?DOES 1
sub isa-nok($test, $expect, $msg) { ok !$test.isa($expect), $msg }

use Grammar;
use Actions;

# XXX remove soon
my $*FILENAME = "bleh";

plan 120;

#?DOES 1
sub parse-block($s) {
    my $a = Pod6::Grammar.parse($s, :actions(Pod6::Actions)).ast;

    isa-ok $a, Pod6::Document, "AST is a Pod6::Document";
    $a[1];
}

# quick note: notice that we test if block *didn't* imply something. To test
# that a block *doesn't* imply something, we need to test the class itself,
# which belongs in a separate test file.

#### =code

my $code = parse-block(qq:to/END_CODE/);
    =code space  preservation
    is in\teffect.
    END_CODE

isa-ok $code, Pod6::Block::Code, "=code produces a Pod6::Block::Code object";
isa-ok $code[0], Pod6::Config, "=code gets an initial Pod6::Config object";
isa-nok $code[1], Pod6::Block::Para, "=code didn't imply =para";
isa-nok $code[1], Pod6::Block::Code, "=code didn't imply =code";

is $code.text, qq:to/END_TEST_CODE/, "=code is space-preserved";
    space  preservation
    is in\teffect.
    END_TEST_CODE

$code = parse-block("=code B<things> are V<verbatim>.");

nok $code.list.any ~~ Pod6::Text::FCode, "=code is verbatim (doesn't parse format codes)";

#### =comment

my $comment = parse-block(q:to/END_COMMENT/);
    =comment no  space
     preservation          should happen
    in this.
    END_COMMENT

isa-ok $comment, Pod6::Block::Comment, "=comment produces a Pod6::Block::Comment object";
isa-ok $comment[0], Pod6::Config, "=comment gets an initial Pod6::Config object";
isa-nok $comment[1], Pod6::Block::Para, "=comment didn't imply =para";
isa-nok $comment[1], Pod6::Block::Code, "=comment didn't imply =code";

is $comment.text, "no space preservation should happen in this.", "=comment isn't space-preserved";

$comment = parse-block("=comment B<Formatting> codes are I<parsed>.");

ok $code.list.any ~~ Pod6::Text::FCode, "=comment isn't verbatim (parses format codes)";

skip("Formatting codes NYI", 2);
#`{
    my @fcodes = $code.list.grep(* ~~ Pod6::Text::FCode);

    isa-ok @fcodes[0], Pod6::Text::FCode::B, "B<> code parsed as a Pod6::Text::FCode::B";
    isa-ok @fcodes[1], Pod6::Text::FCode::I, "I<> code parsed as a Pod6::Text::FCode::I";
}

#### =defn

my $defn = parse-block(q:to/END_DEFN/);
    =defn my term
    I am defining stuff, I'm an   implied para!
    END_DEFN

isa-ok $defn, Pod6::Block::Defn, "=defn produces a Pod6::Block::Defn object";
isa-ok $defn[0], Pod6::Config, "=defn gets an initial Pod6::Config object";
isa-ok $defn[1], Pod6::Block::Para, "=defn did imply =para";

skip("defn.term NYI", 2);
#ok $defn.term, "=defn stores the term line separately";
#isa-ok $defn.term, Pod6::Block::Para, "=defn term is stored as a paragraph";

$defn = parse-block(q:to/END_DEFN/);
    =defn
        my term
        More definitions, but it's an implied code!
    END_DEFN

isa-ok $defn, Pod6::Block::Defn, "=defn with code blocks still produces a Pod6::Block::Defn";
isa-ok $defn[0], Pod6::Config, "=defn with code blocks still gets an initial config";
isa-ok $defn[1], Pod6::Block::Code, "=defn did imply =code";

skip("defn.term NYI", 1);
#isa-ok $defn.term, Pod6::Block::Code, "=defn term is stored as a code block";

#### =head*

my $head = parse-block(q:to/END_HEAD/);
    =head1 my heading  not
           space-preserved,    hopefully.
    END_HEAD

isa-ok $head, Pod6::Block::Head, "=head1 produces a Pod6::Block::Head object";
isa-ok $head[0], Pod6::Config, "=head1 gets an initial Pod6::Config object";
isa-nok $head[1], Pod6::Block::Para, "=head1 didn't imply =para";
isa-nok $head[1], Pod6::Block::Code, "=head1 didn't imply =code";

is $head.level, 1, "=head1 is set at level 1";
is $head.text, "my heading not space-preserved, hopefully.", "=head1 isn't space-preserved";

$head = parse-block("=head3  and some C<formatting> codes, which U<should> be parsed.");

is $head.level, 3, "=head3 is set at level 3";

ok $head.list.any ~~ Pod6::Text::FCode, "=head3 parses formatting codes (isn't verbatim)";

skip("format codes NYI", 2);
#`{
    my @fcodes = $head.list.grep(* ~~ Pod6::Text::FCode);

    isa-ok @fcodes[0], Pod6::Text::FCode::C, "C<> code parsed as a Pod6::Text::FCode::C";
    isa-ok @fcodes[1], Pod6::Text::FCode::U, "U<> code parsed as a Pod6::Text::FCode::U";
}

#### =input

my $input = parse-block(q:to/END_INPUT/);
    =input This is some input,
    and it   is most certainly  space-preserved.
    END_INPUT

isa-ok $input, Pod6::Block::Input, "=input produces a Pod6::Block::Input object";
isa-ok $input[0], Pod6::Config, "=input gets an initial Pod6::Config object";
isa-nok $input[1], Pod6::Block::Para, "=input didn't imply =para";
isa-nok $input[1], Pod6::Block::Code, "=input didn't imply =code";

is $input.text, q:to/END_TEST_INPUT/, "=input is space-preserved";
    This is some input,
    and it   is most certainly  space-preserved.
    END_TEST_INPUT

$input = parse-block("=input Does accept B<format> I<codes>.");

ok $input.list.any ~~ Pod6::Text::FCode, "=input parses formatting codes";

skip("format codes NYI", 2);
#`{
    my @fcodes = $input.list.grep(* ~~ Pod6::Text::FCode);

    isa-ok @fcodes[0], Pod6::Text::FCode::B, "B<> code parsed as a Pod6::Text::FCode::B";
    isa-ok @fcodes[1], Pod6::Text::FCode::I, "I<> code parsed as a Pod6::Text::FCode::I";
}

#### =item

my $item = parse-block(q:to/END_ITEM/);
    =item My item,  not
       space-preserved.
    END_ITEM

isa-ok $item, Pod6::Block::Item, "=item produces a Pod6::Block::Item object";
isa-ok $item[0], Pod6::Config, "=item gets an initial Pod6::Config object";
isa-ok $item[1], Pod6::Block::Para, "=item did imply =para";

is $item.level, 1, "=item is set at level 1";

$item = parse-block(q:to/END_ITEM/);
    =item5
        An indented block,  meaning that
        it's a code block.
    END_ITEM

isa-ok $item, Pod6::Block::Item, "=item5 with code block produces a Pod6::Block::Item object";
isa-ok $item[0], Pod6::Config, "=item5 with code block gets an initial Pod6::Config object";
isa-ok $item[1], Pod6::Block::Code, "=item5 did imply =code";

is $item.level, 5, "=item5 is set at level 5";

#### =nested

my $nested = parse-block(q:to/END_NESTED/);
    =nested A nested block,
    where this is an implicit para
    END_NESTED

isa-ok $nested, Pod6::Block::Nested, "=nested produces a Pod6::Block::Nested object";
isa-ok $nested[0], Pod6::Config, "=nested gets an initial Pod6::Config object";
isa-ok $nested[1], Pod6::Block::Para, "=nested did imply =para";

$nested = parse-block(q:to/END_NESTED/);
    =nested
        A nested block,
        but  this  one  is an  implicit code.
    END_NESTED

isa-ok $nested, Pod6::Block::Nested, "=nested with code block produces a Pod6::Block::Nested object";
isa-ok $nested[0], Pod6::Config, "=nested with code block gets an initial Pod6::Config object";
isa-ok $nested[1], Pod6::Block::Code, "=nested did imply =code";

#### =output

my $output = parse-block(q:to/END_OUTPUT/);
    =output A non-implicit-using block
    which  is  nonetheless
        space-preserved.
    END_OUTPUT

isa-ok $output, Pod6::Block::Output, "=output produces a Pod6::Block::Output object";
isa-ok $output[0], Pod6::Config, "=output gets an initial Pod6::Config object";
isa-nok $output[1], Pod6::Block::Para, "=output didn't imply =para";
isa-nok $output[1], Pod6::Block::Code, "=output didn't imply =code";

is $output.text, q:to/END_TEST_OUTPUT/, "=output is space-preserved";
    A non-implicit-using block
    which  is  nonetheless
        space-preserved.
    END_TEST_OUTPUT

$output = parse-block("=output And yet I<does> allow C<formatting codes>.");

ok $output.list.any ~~ Pod6::Text::FCode, "=output allows formatting codes";

skip("format codes NYI", 2);
#`{
    my @fcodes = $output.list.grep(* ~~ Pod6::Text::FCode);

    isa-ok @fcodes[0], Pod6::Text::FCode::I, "I<> code parsed as Pod6::Text::FCode::I";
    isa-ok @fcodes[1], Pod6::Text::FCode::C, "C<> code parsed as Pod6::Text::FCode::C";
}

#### =para

my $para = parse-block(q:to/END_PARA/);
    =para A bare paragraph,
    implies nothing but still  no space preservation.
    END_PARA

isa-ok $para, Pod6::Block::Para, "=para produces a Pod6::Block::Para object";
isa-ok $para[0], Pod6::Config, "=para gets an initial Pod6::Config object";
isa-nok $para[1], Pod6::Block::Para, "=para didn't imply =para";
isa-nok $para[1], Pod6::Block::Code, "=para didn't imply =code";

is $para.text, "A bare paragraph, implies nothing but still no space preservation.", "=para doesn't preserve spaces";

$para = parse-block("=para And of course, I<all> formatting codes allowed in a R<para>.");

ok $para.list.any ~~ Pod6::Text::FCode, "=para allows formatting codes";

skip("format codes NYI", 2);
#`{
    my @fcodes = $output.list.grep(* ~~ Pod6::Text::FCode);

    isa-ok @fcodes[0], Pod6::Text::FCode::I, "I<> code parsed as Pod6::Text::FCode::I";
    isa-ok @fcodes[1], Pod6::Text::FCode::R, "R<> code parsed as Pod6::Text::FCode::R";
}

#### =pod

my $pod = parse-block(q:to/END_POD/);
    =pod A pod block, which implies paragraphs.
    END_POD

isa-ok $pod, Pod6::Block::Pod, "=pod produces a Pod6::Block::Pod object";
isa-ok $pod[0], Pod6::Config, "=pod gets an initial Pod6::Config object";
isa-ok $pod[1], Pod6::Block::Para, "=pod did imply =para";

$pod = parse-block(q:to/END_POD/);
    =pod
        This pod block has an implied code block.
    END_POD

isa-ok $pod, Pod6::Block::Pod, "=pod with a code block produces a Pod6::Block::Pod object";
isa-ok $pod[0], Pod6::Config, "=pod with a code block gets an initial Pod6::Config object";
isa-ok $pod[1], Pod6::Block::Code, "=pod did imply =code";

#### =table would be here, if not in a separate file

#### =data

my $data = parse-block(q:to/END_DATA/);
    =data
    This is a  simple  data block,
    which is space-preserved, and doesn't imply anything.
    END_DATA

isa-ok $data, Pod6::Block::Data, "=data produces a Pod6::Block::Data object";
isa-ok $data[0], Pod6::Config, "=data gets an initial Pod6::Config object";
isa-nok $data[0], Pod6::Block::Para, "=data didn't imply =para";
isa-nok $data[0], Pod6::Block::Code, "=data didn't imply =code";

is $data.text, q:to/END_TEST_DATA/, "=data preserves spaces";
    This is a  simple  data block,
    which is space-preserved, and doesn't imply anything.
    END_TEST_DATA

# note: the spec is very light on the details of =data. So I've made the
# decision that it allows formatting codes, but it could easily be verbatim
# instead.

$data = parse-block("=data Accepts I<formatting> codes N<at least for now>.");

ok $data.list.any ~~ Pod6::Text::FCode, "=data accepts formatting codes";

skip("format codes NYI", 2);
#`{
    my @fcodes = $data.list.grep(* ~~ Pod6::Text::FCode);

    isa-ok @fcodes[0], Pod6::Text::FCode::I, "I<> code parsed as Pod6::Text::FCode::I";
    isa-ok @fcodes[1], Pod6::Text::FCode::N, "N<> code parsed as Pod6::Text::FCode::N";
}

#### =finish

# note: if/when transforming this file for roast (and thus testing the parsing
# of "actual" blocks in code), all but one of these =finish tests _must_ be
# EVAL'd.

my $finish = parse-block(q:to/END_FINISH/);
    =finish This is a finish block,
    which is basically like =pod, so this is an implied para.
    END_FINISH

isa-ok $finish, Pod6::Block::Finish, "=finish produces a Pod6::Block::Finish object";
isa-ok $finish[0], Pod6::Config, "=finish gets an initial Pod6::Config object";
isa-ok $finish[1], Pod6::Block::Para, "=finish did imply =para";

$finish = parse-block(q:to/END_FINISH/);
    =finish
        This finish block implies code blocks, also just like =pod.
    END_FINISH

isa-ok $finish, Pod6::Block::Finish, "=finish with code block produces a Pod6::Block::Finish object";
isa-ok $finish[0], Pod6::Config, "=finish with code block gets an initial Pod6::Config object";
isa-ok $finish[1], Pod6::Block::Code, "=finish did imply =code";

## testing the special EOF-only behavior of =finish

skip("Special =finish behavior NYI", 5);
#$finish = parse-block(q:to/END_FINISH/);
#    =begin finish
#    This shows that you don't need an =end finish.
#    END_FINISH

#isa-ok $finish, Pod6::Block::Finish, "=begin finish doesn't need an =end";

#$finish = parse-block(q:to/END_FINISH/);
#    =begin finish
#    And this shows you how an =end finish gets ignored (and warned about).
#    =end finish
#    Still finishing!
#    END_FINISH

#isa-ok $finish, Pod6::Block::Finish, "=begin finish with superfluous =end";
#isa-ok $finish.text, q:to/END_TEST_FINISH/, "=begin finish ignores and continues past a superfluous =end";
#    And this shows you how an =end finish gets ignored (and warned about).
#    =end finish
#    Still finishing!
#    END_TEST_FINISH