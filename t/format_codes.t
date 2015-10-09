# format_codes.t --- Tests all the various formatting codes.

use v6;
use Test;

use Grammar;
use Actions;

#?DOES 2
sub parse-fc($pod) {
    my $a = Pod6::Grammar.parse($pod, :actions(Pod6::Actions)).ast;

    isa-ok $a, Pod6::Document, "Returned AST is a Pod6::Document";

    $a = $a[1];

    isa-ok $a, Pod6::Block::Para, "Returned Document has just a Pod6::Block::Para";

    $a[1];
}

my $FC;

####
#### A
####

skip("A<> NYI", 9);
#`⟅
$FC = Pod6::Grammar.parse(q:to/END_A/, :actions(Pod6::Actions));
    =alias foo BAR
    =para A<foo>
    END_A

isa-ok $FC, Pod6::Document, "Returned AST is a Pod6::Document";

$FC = $FC[2][1]; # get code inside =para

isa-ok $FC, Pod6::Text::FormatCode::A, "A<> code is of type Pod6::Text::FormatCode::A";
is $FC.text, "BAR", "A<> code is substituted correctly";

$FC = Pod6::Grammar.parse(q:to/END_A/, :actions(Pod6::Actions));
    =alias    foo WRONG
    =alias B<foo> RIGHT
    =para A«B<foo>»
    END_A

$FC = $FC[2][1];

isa-ok $FC, Pod6::Text::FormatCode::A, "A«» code is of type Pod6::Text::FormatCode::A";
is $FC.text, "RIGHT", "A«» code is verbatim, and doesn't parse formatting codes";

$FC = parse-fc('=para A<<$?FILE>>');

isa-ok $FC, Pod6::Text::FormatCode::A, "A<<>> code is of type Pod6::Text::FormatCode::A";
is $FC.text, $?FILE, "A<<>> code substituted compile-time constant correctly";
⟆

# XXX more A<> tests (involving pure-only method calls on stuff)

####
#### B
####

$FC = parse-fc("=para B<Bolding text>");

isa-ok $FC, Pod6::Text::FormatCode::B, "B<> code is of type Pod6::Text::FormatCode::B";
is $FC.text, "Bolding text", "B<> code has correct text";

$FC = parse-fc("=para B«more    bold!»");

isa-ok $FC, Pod6::Text::FormatCode::B, "B«» code is of type Pod6::Text::FormatCode::B";
is $FC.text, "more bold!", "B«» code doesn't preserve space";

$FC = parse-fc("=para B<<<B««yet more»» bold>>>");

isa-ok $FC, Pod6::Text::FormatCode::B, "B<<<>>> code is of type Pod6::Text::FormatCode::B";
isa-ok $FC[0], Pod6::Text::FormatCode::B, "B<<<>>> handles nested B««»»";
is $FC[0].text, "yet more", "Nested B««»» has right text";
isa-ok $FC[1], Pod6::Text::Plain, "Rest of B<<<>>> is plain text";
is $FC[1].text, " bold", "Rest of B<<<>>> is the correct text";

####
#### C
####

$FC = parse-fc("=para C<some code>");

isa-ok $FC, Pod6::Text::FormatCode::C, "C<> code is of type Pod6::Text::FormatCode::C";
is $FC.text, "some code", "C<> code has correct text";

$FC = parse-fc("=para C«    indented  code»");

isa-ok $FC, Pod6::Text::FormatCode::C, "C«» code is of type Pod6::Text::FormatCode::C";
is $FC.text, "    indented  code", "C«» preserves spaces";

$FC = parse-fc("=para C««  forever C<<<<code>>>>  »»");

isa-ok $FC, Pod6::Text::FormatCode::C, "C««»» code is of type Pod6::Text::FormatCode::C";
is $FC.elems, 1, "C««»» doesn't parse embedded formatting codes";
is $FC.text, "  forever C<<<<code>>>>  ", "C««»» has correct text";

####
#### D
####

$FC = parse-fc("=para D<defining this>");

isa-ok $FC, Pod6::Text::FormatCode::D, "D<> code is of type Pod6::Text::FormatCode::D";
is $FC.text, "defining this", "D<> code has correct text";
is $FC.term, "defining this", "D<> code has proper .term";
is $FC.synonyms.elems, 0, "D<> code has no synonyms";

$FC = parse-fc("=para D«  and	this | syn»");

isa-ok $FC, Pod6::Text::FormatCode::D, "D«» code is of type Pod6::Text::FormatCode::D";
is $FC.text, "and this", "D«» code doesn't preserve spaces";
is $FC.text, $FC.term, "D«» has equal 'text' and 'term'";
is $FC.synonyms.elems, 1, "D«» has one synonym";
is $FC.synonyms[0], "syn", "D«» doesn't preserve spaces in synonym";

$FC = parse-fc("=para D<<one more|term;but with; more synonyms>>");

isa-ok $FC, Pod6::Text::FormatCode::D, "D<<>> code is of type Pod6::Text::FormatCode::D";
is $FC.text, "one more", "D<<>> has correct text";
is $FC.term, $FC.text, "D<<>> has equal 'text' and 'term'";
is $FC.synonyms.elems, 3, "D<<>> has three synonyms";
is-deeply $FC.synonyms.List, «term "but with" "more synonyms"», "D<<>> has correct synonyms";

####
#### E
####

$FC = parse-fc("=para E<0x2603>");

isa-ok $FC, Pod6::Text::FormatCode::E, "E<> code is of type Pod6::Text::FormatCode::E";
is $FC.text, "☃", "E<> code produces the right character";

$FC = parse-fc("=para E«9731»");

isa-ok $FC, Pod6::Text::FormatCode::E, "E«» code is of type Pod6::Text::FormatCode::E";
is $FC.text, "☃", "E«» code produces the right character";

$FC = parse-fc("=para E<<SNOWMAN>>");

isa-ok $FC, Pod6::Text::FormatCode::E, "E<<>> code is of type Pod6::Text::FormatCode::E";
is $FC.text, "☃", "E<<>> code produces the right character";

$FC = parse-fc("=para E««0o23003; 0d9732 ;0b0010_0110_0000_0011; COMET»»");

isa-ok $FC, Pod6::Text::FormatCode::E, "E««»» code is of type Pod6::Text::FormatCode::E";
is $FC.text, "☃☄☃☄", "E««»» code produces the right characters";

# TOCORE should be an eval-dies-ok
dies-ok { Pod6::Grammar.parse("=para E<amp>", :actions(Pod6::Actions)) },
        "Can't use HTML5 character references in E<> (old behavior)";


####
#### F
####

dies-ok { Pod6::Grammar.parse("=para F<>", :actions(Pod6::Actions)) },
        "F<> is reserved";

####
#### G
####

dies-ok { Pod6::Grammar.parse("=para G<>", :actions(Pod6::Actions)) },
        "G<> is reserved";

####
#### H
####

dies-ok { Pod6::Grammar.parse("=para H<>", :actions(Pod6::Actions)) },
        "H<> is reserved";

####
#### I
####

$FC = parse-fc("=para I<Italicizing text>");

isa-ok $FC, Pod6::Text::FormatCode::I, "I<> code is of type Pod6::Text::FormatCode::I";
is $FC.text, "Italicizing text", "I<> code has correct text";

$FC = parse-fc("=para I«more    italic!»");

isa-ok $FC, Pod6::Text::FormatCode::I, "I«» code is of type Pod6::Text::FormatCode::I";
is $FC.text, "more italic!", "I«» code doesn't preserve space";

$FC = parse-fc("=para I<<<I««yet more»» obliqueness>>>");

isa-ok $FC, Pod6::Text::FormatCode::I, "I<<<>>> code is of type Pod6::Text::FormatCode::I";
isa-ok $FC[0], Pod6::Text::FormatCode::I, "I<<<>>> handles nested I««»»";
is $FC[0].text, "yet more", "Nested I««»» has right text";
isa-ok $FC[1], Pod6::Text::Plain, "Rest of I<<<>>> is plain text";
is $FC[1].text, " obliqueness", "Rest of I<<<>>> is the correct text";

####
#### J
####

dies-ok { Pod6::Grammar.parse("=para J<>", :actions(Pod6::Actions)) },
        "J<> is reserved";

####
#### K
####

$FC = parse-fc("=para K<keyboard input>");

isa-ok $FC, Pod6::Text::FormatCode::K, "K<> code is of type Pod6::Text::FormatCode::K";
is $FC.text, "keyboard input", "K<> code has correct text";

$FC = parse-fc("=para K«  more input »"); # last ws char is U+2003 EM SPACE

isa-ok $FC, Pod6::Text::FormatCode::K, "K«» code is of type Pod6::Text::FormatCode::K";
is $FC.text, "  more input ", "K<> preserves text";

$FC = parse-fc("=para K««  B<important> input»»");

isa-ok $FC, Pod6::Text::FormatCode::K, "K««»» code is of type Pod6::Text::FormatCode::K";
isa-ok $FC[0], Pod6::Text::Plain, "Space before nested code is kept";
is $FC[0].text, "  ", "Spaces preserved before nested code";
isa-ok $FC[1], Pod6::Text::FormatCode::B, "K««»» parses nested B<> code";
is $FC[1].text, "important", "Nested B<> code has correct text";
isa-ok $FC[2], Pod6::Text::Plain, "Rest of K««»» is plain text";
is $FC[2].text, " input", "Rest of K««»» has correct text";

####
#### L
####

$FC = parse-fc("=para L<doc:perl6var>");

isa-ok $FC, Pod6::Text::FormatCode::L, "L<> code is of type Pod6::Text::FormatCode::L";
isa-ok $FC, Pod6::Text::FormatCode::L::Doc, "L<doc:...> code is of type Pod6::Text::FormatCode::L::Doc";
is $FC.text, "doc:perl6var", "L<> code has default display text";
is $FC.scheme, "doc", "L<> reports proper scheme name";
is $FC.address, "perl6var", "L<> reports whole address after scheme";
is $FC.link, "doc:perl6var", "L<> link provides scheme + address";

# ::Doc specific
is $FC.doc, "perl6var", "L<doc:...> code specifies correct document";

## note: this 'subsection' reference syntax is conjectural
$FC = parse-fc("=para L«click here! | doc:S26#head# Pod#Block    types#Lists»");

isa-ok $FC, Pod6::Text::FormatCode::L, "L«» code is of type Pod6::Text::FormatCode::L";
isa-ok $FC, Pod6::Text::FormatCode::L::Doc, "L«doc:...» code is of type Pod6::Text::FormatCode::L::Doc";
is $FC.text, "click here!", "L«» code has correct display text";
is $FC.scheme, "doc", "L«» reports proper scheme name";
is $FC.address, "S26#head# Pod#Block    types#Lists", "L«» reports correct whole address, space-preserved";
is $FC.link, "doc:S26#head# Pod#Block    types#Lists", "L«» link is scheme + address";

# ::Doc specific
is $FC.doc, "S26", "L«doc:...» specifies correct document";
is $FC.inner-by, "head", "L«doc:...» internal address info specifies how to resolve said address";
is-deeply $FC.inner.List, «Pod  "Block types"  Lists», "L«doc:...» correctly interpreted internal address (not space-preserved)";

$FC = parse-fc("=para L<<more display|http://docs.perl6.org/type/Pod::Block#Methods>>");

isa-ok $FC, Pod6::Text::FormatCode::L, "L<<>> code is of type Pod6::Text::FormatCode::L";
isa-ok $FC, Pod6::Text::FormatCode::L::Http, "L<<http:...>> code is of type Pod6::Text::FormatCode::L::Http";
is $FC.text, "more display", "L<<>> code has correct display text";
is $FC.scheme, "http", "L<<>> code gives correct scheme name";
is $FC.address, "//docs.perl6.org/type/Pod::Block#Methods", "L<<>> code gives correct whole address";
is $FC.link, "http://docs.perl6.org/type/Pod::Block#Methods", "L<<>> link gives scheme + address";

# ::Http specific
is $FC.external, "//docs.perl6.org/type/Pod::Block", "L<<http:...>> gives correct external address";
is $FC.internal, "Methods", "L<<http:...>> gives correct internal address";

# more tests on L's standard schemes should be in a separate test file

## test scheme without predefined class (should ideally 'autogen' a default
## class).

skip('Autogen classes NYI', 8);
#$FC = parse-fc("=para L«««xyzzy:my-own#address-thing»»»");

#isa-ok $FC, Pod6::Text::FormatCode::L, "L«««»»» code is of type Pod6::Text::FormatCode::L";
#isa-ok $FC, Pod6::Text::FormatCode::L::Xyzzy, "L«««xyzzy:...»»» is autogen'd subclass Pod6::Text::FormatCode::L::Xyzzy";
#is $FC.text, "xyzzy:my-own#address-thing", "L«««»»» has correct display text";
#is $FC.scheme, "xyzzy", "L«««»»» has correct scheme name";
#is $FC.address, "my-own#address-thing", "L«««»»» has correct address";
#is $FC.link, "xyzzy:my-own#address-thing", "L«««»»» has correct link";

####
#### M
####

class Pod6::Text::FormatCode::M::Foobar is Pod6::Text::FormatCode::M {
    method text { "I'm a custom M<> code!" }
}

$FC = parse-fc("=para M<foobar:some V<verbatim>, space-preserved text>");

isa-ok $FC, Pod6::Text::FormatCode::M, "M<> code is of type Pod6::Text::FormatCode::M";
isa-ok $FC, Pod6::Text::FormatCode::M::Foobar, "M<foobar:...> is of type Pod6::Text::FormatCode::M::Foobar";
is $FC.verbatim, "some V<verbatim>, space-preserved text", "M<> provides verbatim text";
is $FC.text, "I'm a custom M<> code!", "M<> provides expected text";

skip('Autogen classes NYI', 6);
#$FC = parse-fc("=para M«xyzzy: another piece of M«» text»");

#isa-ok $FC, Pod6::Text::FormatCode::M, "M«» code is of type Pod6::Text::FormatCode::M";
#isa-ok $FC, Pod6::Text::FormatCode::M::Xyzzy, "M«xyzzy:...» is of autogen'd type Pod6::Text::FormatCode::M::Xyzzy";
#is $FC.verbatim, " another piece of M«» text", "M«» gives correct verbatim text";
#is $FC.text, " another piece of M«» text", "M«» has correct default display text";

####
#### N
####

$FC = parse-fc("=para N<footnote>");

isa-ok $FC, Pod6::Text::FormatCode::N, "N<> code is of type Pod6::Text::FormatCode::N";
is $FC.text, "footnote", "N<> has correct text";

$FC = parse-fc("=para N«  another  footnote»");

isa-ok $FC, Pod6::Text::FormatCode::N, "N«» code is of type Pod6::Text::FormatCode::N";
is $FC.text, "another footnote", "N«» doesn't preserve spaces";

$FC = parse-fc("=para N<<<<<< one more N«« footnote »»>>>>>>");

isa-ok $FC, Pod6::Text::FormatCode::N, "N<<<<<<>>>>>> code is of type Pod6::Text::FormatCode::N";
isa-ok $FC[0], Pod6::Text::Plain, "N<<<<<<>>>>>> code starts with plaintext";
is $FC[0].text, "one more ", "Text before nested code is correct";
isa-ok $FC[1], Pod6::Text::FormatCode::N, "N<<<<<<>>>>>> handles nested formatting code";
is $FC[1].text, "footnote", "Nested code has correct text";

####
#### O
####

dies-ok { Pod6::Grammar.parse("=para O<>", :actions(Pod6::Actions)) },
        "O<> is reserved";

####
#### P
####

$FC = parse-fc("=para P<doc:A02>");

isa-ok $FC, Pod6::Text::FormatCode::P, "P<> code is of type Pod6::Text::FormatCode::P";
isa-ok $FC, Pod6::Text::FormatCode::P::Doc, "P<doc:...> code is of type Pod6::Text::FormatCode::P::Doc";
is $FC.scheme, "doc", "P<> code gives correct scheme name";
is $FC.address, "A02", "P<> code gives correct address";
is $FC.link, "doc:A02", "P<> code gives correct link";
dies-ok { $FC.text }, "P<> can't return .text, since it's supposed to be retrieved by the renderer";
# the above test about .text is just conjectured behavior atm.

skip('Autogen classes NYI', 7);
#$FC = parse-fc("=para P«xyzzy:place-thing»");

#isa-ok $FC, Pod6::Text::FormatCode::P, "P«» code is of type Pod6::Text::FormatCode::P";
#isa-ok $FC, Pod6::Text::FormatCode::P::Xyzzy, "P«» code is of autogen'd type Pod6::Text::FormatCode::P::Xyzzy";
#is $FC.scheme, "xyzzy", "P«» code gives correct scheme name";
#is $FC.address, "place-thing", "P«» code gives correct address";
#is $FC.link, "xyzzy:place-thing", "P«» code gives correct link";

# standard P<> schemes tested separately

####
#### Q
####

dies-ok { Pod6::Grammar.parse("=para Q<>", :actions(Pod6::Actions)) },
        "Q<> is reserved";

####
#### R
####

$FC = parse-fc("=para R<replaceable>");

isa-ok $FC, Pod6::Text::FormatCode::R, "R<> code is of type Pod6::Text::FormatCode::R";
is $FC.text, "replaceable", "R<> has correct text";

$FC = parse-fc("=para R«disposable    »");

isa-ok $FC, Pod6::Text::FormatCode::R, "R«» code is of type Pod6::Text::FormatCode::R";
is $FC.text, "disposable", "R«» doesn't preserve spaces";

$FC = parse-fc("=para R««B<really> expendable»»");

isa-ok $FC, Pod6::Text::FormatCode::R, "R««»» code is of type Pod6::Text::FormatCode::R";
isa-ok $FC[0], Pod6::Text::FormatCode::B, "R««»» handles nested codes";
is $FC[0].text, "really", "Nested code has correct text";
isa-ok $FC[1], Pod6::Text::Plain, "Rest of R««»» is plaintext";
is $FC[1].text, " expendable", "Rest of R««»» has correct text";

####
#### S
####

$FC = parse-fc("=para S<spacey text>");

isa-ok $FC, Pod6::Text::FormatCode::S, "S<> code is of type Pod6::Text::FormatCode::S";
is $FC.text, "spacey text", "S<> has correct text";

$FC = parse-fc("=para S«  save these   spaces!»");

isa-ok $FC, Pod6::Text::FormatCode::S, "S«» code is of type Pod6::Text::FormatCode::S";
is $FC.text, "  save these   spaces!", "S«» code preserves spaces";

$FC = parse-fc("=para S<<<< allows S««  codes»»>>>>");

isa-ok $FC, Pod6::Text::FormatCode::S, "S<<<<>>>> code is of type Pod6::Text::FormatCode::S";
isa-ok $FC[0], Pod6::Text::Plain, "S<<<<>>>> starts as plaintext";
is $FC[0].text, " allows ", "Start of S<<<<>>>> code has correct text";
isa-ok $FC[1], Pod6::Text::FormatCode::S, "S<<<<>>>> handles nested S««»» code";
is $FC[1].text, "  codes", "Nested code has correct text";

####
#### T
####

$FC = parse-fc("=para T<terminal output>");

isa-ok $FC, Pod6::Text::FormatCode::T, "T<> code is of type Pod6::Text::FormatCode::T";
is $FC.text, "terminal output", "T<> code has correct text";

$FC = parse-fc("=para T«  more output »"); # last ws char is U+2003 EM SPACE

isa-ok $FC, Pod6::Text::FormatCode::T, "T«» code is of type Pod6::Text::FormatCode::T";
is $FC.text, "  more output ", "T<> preserves text";

$FC = parse-fc("=para T««  B<important> output»»");

isa-ok $FC, Pod6::Text::FormatCode::T, "T««»» code is of type Pod6::Text::FormatCode::T";
isa-ok $FC[0], Pod6::Text::Plain, "Space before nested code is kept";
is $FC[0].text, "  ", "Spaces preserved before nested code";
isa-ok $FC[1], Pod6::Text::FormatCode::B, "T««»» parses nested B<> code";
is $FC[1].text, "important", "Nested B<> code has correct text";
isa-ok $FC[2], Pod6::Text::Plain, "Rest of T««»» is plain text";
is $FC[2].text, " output", "Rest of T««»» has correct text";

####
#### U
####

$FC = parse-fc("=para U<Underlining text>");

isa-ok $FC, Pod6::Text::FormatCode::U, "U<> code is of type Pod6::Text::FormatCode::U";
is $FC.text, "Underlining text", "U<> code has correct text";

$FC = parse-fc("=para U«more    underline!»");

isa-ok $FC, Pod6::Text::FormatCode::U, "U«» code is of type Pod6::Text::FormatCode::U";
is $FC.text, "more underline!", "U«» code doesn't preserve space";

$FC = parse-fc("=para U<<<U««yet more»» beneath-text-lines>>>");

isa-ok $FC, Pod6::Text::FormatCode::U, "U<<<>>> code is of type Pod6::Text::FormatCode::U";
isa-ok $FC[0], Pod6::Text::FormatCode::U, "U<<<>>> handles nested U««»»";
is $FC[0].text, "yet more", "Nested U««»» has right text";
isa-ok $FC[1], Pod6::Text::Plain, "Rest of U<<<>>> is plain text";
is $FC[1].text, " beneath-text-lines", "Rest of U<<<>>> is the correct text";

####
#### V
####

$FC = parse-fc("=para V<text verbatim>");

isa-ok $FC, Pod6::Text::FormatCode::V, "V<> code is of type Pod6::Text::FormatCode::V";
is $FC.text, "text verbatim", "V<> code has correct text";

$FC = parse-fc("=para V« don't  preserve    spaces»");

isa-ok $FC, Pod6::Text::FormatCode::V, "V«» code is of type Pod6::Text::FormatCode::V";
is $FC.text, "don't preserve spaces", "V«» doesn't preserve spaces";

$FC = parse-fc("=para V<<<< ignores V««formatting»» codes >>>>");

isa-ok $FC, Pod6::Text::FormatCode::V, "V<<<<>>>> code is of type Pod6::Text::FormatCode::V";
is $FC.elems, 1, "V<<<<>>>> has just one element";
isa-ok $FC[0], Pod6::Text::Plain, "V<<<<>>>> has just plaintext";
is $FC.text, "ignores V««formatting»» codes", "V<<<<>>>> has correct text";

####
#### W
####

dies-ok { Pod6::Grammar.parse("=para W<>", :actions(Pod6::Actions)) },
        "W<> is reserved";

####
#### X
####

$FC = parse-fc("=para X<a term>");

isa-ok $FC, Pod6::Text::FormatCode::X, "X<> code is of type Pod6::Text::FormatCode::X";
is $FC.text, "a term", "X<> code has correct display text";
is $FC.entries.elems, 1, "X<> code has one entry";
ok $FC.entries{"a term"}, "X<> code has right entry";

$FC = parse-fc("=para X«  display  text | entry »");

isa-ok $FC, Pod6::Text::FormatCode::X, "X«» code is of type Pod6::Text::FormatCode::X";
is $FC.text, "display text", "X«» code has correct (non-space-preserved) text";
is $FC.entries.elems, 1, "X«» code has one entry";
ok $FC.entries{"entry"}:exists, "X«» has correct entry";

$FC = parse-fc("=para X<<<display|entry 1, meaning of, first>>>");

isa-ok $FC, Pod6::Text::FormatCode::X, "X<<<>>> code is of type Pod6::Text::FormatCode::X";
is $FC.text, "display", "X<<<>>> has correct display text";
is $FC.entries.elems, 1, "X<<<>>> has one entry";
ok $FC.entries{"entry 1"}:exists, "X<<<>>> entry has correct top-level name";
is $FC.entries{"entry 1"}, "meaning of" => "first" => True, "X<<<>>> entry has correct (non-space-preserved) subentries";

$FC = parse-fc("=para X««entry 1, subent; entry 2; entry 3»»");

isa-ok $FC, Pod6::Text::FormatCode::X, "X««»» code is of type Pod6::Text::FormatCode::X";
is $FC.text, "entry 1", "X««»» has the correct display text";
is $FC.entries.elems, 3, "X««»» has three entries";
ok $FC.entries{"entry 1"}:exists, "X««»» has correct first entry";
ok $FC.entries{"entry 1"}{"subent"}:exists, "X««»» first entry has proper subentry";
ok $FC.entries{"entry 2"}:exists, "X««»» has correct second entry, non-space-preserved";
ok $FC.entries{"entry 3"}:exists, "X««»» has correct third entry";

####
#### Y
####

dies-ok { Pod6::Grammar.parse("=para Y<>", :actions(Pod6::Actions)) },
        "Y<> is reserved";

####
#### Z
####

$FC = parse-fc("=para Z<zero-width comment>");

isa-ok $FC, Pod6::Text::FormatCode::Z, "Z<> code is of type Pod6::Text::FormatCode::Z";
is $FC.text, "", "Z<> code always acts as empty text";
is $FC.hidden, "zero-width comment", "Z<> code keeps contents, as 'hidden' text";

$FC = parse-fc("=para Z«   no        space preserves »");

isa-ok $FC, Pod6::Text::FormatCode::Z, "Z«» code is of type Pod6::Text::FormatCode::Z";
is $FC.text, "", "Z«» code always acts as empty text";
is $FC.hidden, "no space preserves", "Z«» hidden text isn't space-preserved";

$FC = parse-fc("=para Z<<<<handles B<formatting> codes>>>>");

isa-ok $FC, Pod6::Text::FormatCode::Z, "Z<<<<>>>> code is of type Pod6::Text::FormatCode::Z";
is $FC.text, "", "Z<<<<>>>> code always acts as empty text";
isa-ok $FC[0], Pod6::Text::Plain, "First part of code is plaintext";
is $FC[0].text, "handles ", "First part of code has correct text";
isa-ok $FC[1], Pod6::Text::FormatCode::B, "Z<<<<>>>> handles nested codes";
is $FC[1].text, "formatting", "Nested code has correct text";
isa-ok $FC[2], Pod6::Text::Plain, "Third part of code is plaintext";
is $FC[2].text, " codes", "Third part of code has correct text";

$FC = parse-fc("=para Z««zero-width Z«inside zero-width»!»»");

isa-ok $FC, Pod6::Text::FormatCode::Z, "Z««»» code is of type Pod6::Text::FormatCode::Z";
is $FC.text, "", "Z««»» code always acts as empty text";
isa-ok $FC[1], Pod6::Text::FormatCode::Z, "Embedded Z«» inside Z««»»";
is $FC.hidden, "zero-width !", "Z««»» hidden text doesn't show embedded Z«»";
is $FC[1].hidden, "inside zero-width", "Embedded Z«» has correct hidden text";

done-testing;