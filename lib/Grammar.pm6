# Grammar.pm6 --- Pod6 Grammar

use v6;

# since this will eventually be plugged into the rakudo grammar/actions setup,
# we need to be as NQP-y as can be reasonably done. Why not just write it in NQP
# then? Because we wouldn't have access to $*W or the other hooks into Perl6 we
# could have, so better to work in a system with those things than to try vainly
# to work around it while out of core.

use nqp;

# these shims are to explicitly mark stuff that wouldn't be doing this in NQP-land
sub shim-unbox_i(Int $a) { nqp::unbox_i($a) }
sub shim-unbox_s(Str $a) { nqp::unbox_s($a) }

sub shim-box_i(int $a) { nqp::box_i($a, Int) }

use Exception;

# TOCORE used just to get our own error reporting stuff
use Grammar::Parsefail;

#use Grammar::Tracer;

grammar Pod6::Grammar is Grammar::Parsefail {
    token TOP {
        :my @*POD_BLOCKS := nqp::list();
        :my @*FORMAT_CODES := nqp::list();

        :my @*CONFIG_INFO := nqp::list(); # for =config options

        :my @*MARGINS := nqp::list();
        :my $*CODE_MARGIN; # for implied code blocks

        :my $*LAST_BEGIN;

        <.start_document>

        <.blank_line>*
        [<block>
         <.blank_line>*]*

        [ $ || <.typed_panic(X::Pod6::Didn'tComplete)> ]

        <.express_concerns>
    }

    token start_document { <?> }

    # XXX if this grammar is not kept independent in the move to core, this rule
    # has to be renamed
    token ws {
        <!ww> \h*
    }

    # various line handlers, usually called in <.rule> form

    # XXX apparently we're supposed to emulate tab stops in doing this. Gr.
    sub numify-margin($margin) {
        my $de-tabbed := nqp::split("\t", shim-unbox_s($margin));

        if nqp::elems($de-tabbed) == 1 {
            nqp::chars($margin);
        } else {
            nqp::chars(
                nqp::join(
                    nqp::x(
                        " ",
                        ($?TABSTOP // 8)),
                    $de-tabbed));
        }
    }

    method prime_line($margin) {
        nqp::push(@*MARGINS, $margin);
        self;
    }

    method unprime_line {
        nqp::pop(@*MARGINS);
        self;
    }

    method prime_code_line($margin) {
        $*CODE_MARGIN := $margin;
        self;
    }

    token start_line {
        ^^ " " ** { @*MARGINS[*-1] }
    }

    token start_code_line {
        ^^ " " ** { @*MARGINS[*-1] + $*CODE_MARGIN }
    }

    token end_line {
        $$ [\n || $ || <.panic("Unknown line ending found.")>]
    }

    token blank_line {
        ^^ \h* <.end_line>
    }

    token blank_or_eof {
        <.blank_line> || $
    }

    token end_non_delim { <.blank_or_eof> | <?before <.new_directive>> }

    # stuff dealing with configs

    token new_scope { <?> }

    token finish_scope { <?> }

    # high-level block handling

    token block {
        :my $*BLOCK_NAME;
        :my %*THIS_CONFIG; # for the particular block's configuration. Allows
                           # one-shot configs without extra headache.

        ^^ $<litmargin>=(\h*) <?before <new_directive>>
        {self.prime_line(nqp::chars(~$<litmargin>))}

        <directive>

        {self.unprime_line}
    }

    proto token directive {*}

    multi token directive:sym<delim> {
        "=begin" <.ws> # XXX want :: here
            <block_name> <.ws>

        {$*BLOCK_NAME := ~$<block_name>}

        <.new_scope>

        <.block_config>

        $<contents>=( <!before \h* "=end">
          [
          | <block>
          | <.start_line> <pseudopara>
          | <.blank_line>
          ]
        )*

        <.start_line> "=end" <.ws> [$<block_name>
                                   || <badname=.block_name> {$<badname>.CURSOR.typed_panic(X::Pod6::MismatchedEnd, HINT-MATCH => $/)}
                                   ] <.ws> <.end_line>
        {$*LAST_BEGIN = $/} # for hints on extraneous =end blocks

        <.finish_scope>
    }

    multi token directive:sym<para> {
        "=for" <.ws> # XXX want :: here
            <block_name> <.ws>

        {$*BLOCK_NAME := ~$<block_name>}

        <.new_scope>

        <.block_config>

        <.start_line> <pseudopara>

        <.end_non_delim>

        <.finish_scope>
    }

    multi token directive:sym<abbr> {
        \= <!not_name> <block_name> <.ws>

        {$*BLOCK_NAME := ~$<block_name>}

        <.new_scope>

        # implied code blocks can only start on the next line, since only there
        # can we check for indentation. However, since we eat up all the
        # whitespace on the first line (see above <.ws>), we don't need to do
        # anything special.

        [<.end_line> <.start_line>]? <pseudopara>

        <.end_non_delim>

        <.finish_scope>
    }

    multi token directive:sym<encoding> {
        "=encoding" <.ws> #`(::)
        $<encoding>=[\N+ <.end_line>
            [<!blank_or_eof> <.start_line> \N+ <.end_line>]*]

        <.end_non_delim>

        {$¢.typed_worry(X::Pod6::Encoding, target-enc => ~$<encoding>)}
    }

    multi token directive:sym<alias> {
        "=alias" <.ws> <AVal=.p6ident> <.ws>
        [<.end_line> {$¢.typed_panic(X::Pod6::Alias, atype => "Contextual")}]?

        \N+ {$¢.typed_sorry(X::Pod6::Alias, atype => "Macro")}
        <.end_line> <.end_non_delim>
    }

    multi token directive:sym<config> {
        "=config" <.ws> #`(::)
        $<thing>=[<.block_name>|<[A..Z]> "<>"] <.ws>
        <configset> <.end_line>
        <extra_config_line>*
        <.end_non_delim>
    }

    multi token directive:sym<end> {
        "=end" <.ws> <block_name> #`(::)
        {
            if $*LAST_BEGIN {
                $¢.typed_panic(X::Pod6::ExtraEnd, HINT-MATCH => $*LAST_BEGIN);
            } else {
                $¢.typed_panic(X::Pod6::ExtraEnd);
            }
        }
    }

    token block_name {
        || [<standard_name> | <semantic_standard_name>]
        || <not_name> { $¢.typed_panic(X::Pod6::Block::DirectiveAsName, culprit => ~$<not_name>) }
        || <reserved_name> { $¢.typed_sorry(X::Pod6::Block::ReservedName, culprit => ~$<reserved_name>) }
        || <typename>
    }

    token standard_name {
        | code
        | comment
        | data
        | defn
        | finish
        | <(head)> $<level>=[\d+]
        | input
        | <(item)> $<level>=(\d+)?
        | nested
        | output
        | para
        | pod
        | table
    }

    # since S26 states that each name and its plural is reserved, I decided to
    # pluralize even those that don't have a grammatical plural :P . (Remember
    # that NAMES? means NAME[S?], as one alternative spelling.)
    token semantic_standard_name {
        | NAMES?
        | AUTHORS?
        | VERSIONS?
        | CREATEDS?
        | EMULATES[ES]?
        | EXCLUDES[ES]?
        | SYNOPS<[IE]>S
        | DESCRIPTIONS?
        | USAGES?
        | INTERFACES?
        | METHODS?
        | SUBROUTINES?
        | OPTIONS?
        | DIAGNOSTICS?
        | ERRORS?
        | WARNINGS?
        | DEPENDENC[Y|IES]
        | BUGS?
        | SEE\-ALSOS?
        | ACKNOWLEDG[E?]MENTS?
        | COPYRIGHTS?
        | DISCLAIMERS?
        | LICENCES?
        | LICENSES?
        | TITLES?
        | SECTIONS?
        | CHAPTERS?
        | APPENDI[X|CES]
        | TOCS?
        | IND[EX[ES]? | ICES]  # both 'indexes' and 'indices' are valid plurals
        | FOREWORDS?
        | SUMMAR[Y|IES]
    }

    token not_name {
        | begin
        | for
        | end
        | config
        | alias
        | encoding
    }

    token reserved_name {
        [
        | [<:Lower>|_] [<['-]>? [<:Lower>|_|\d]]*
        | [<:Upper>|_] [<['-]>? [<:Upper>|_|\d]]+
        ]
        <!ww>
    }

    token typename {
        <p6ident>
        <!{ ~$<p6ident> eq (~$<p6ident>).uc || ~$<p6ident> eq (~$<p6ident>).lc }>
    }

    token block_config {
        <configset>? <.end_line>
        <extra_config_line>*
    }

    proto token config_option {*}

    multi token config_option:sym<colonpair> {
        ':'
        [
        | $<neg>=['!'] <key=.p6ident>
        | <?[$@%&]> <.typed_panic(X::Pod6::BadConfig, message => "Attempted to use variable as colonpair; only constants are allowed in Pod configuration")>
        | <key=.p6ident> [$<value>=['(' ~ ')' [<podint>|<podstr>] | <cgroup>]]?
        ]
    }

    multi token config_option:sym<fatarrow> {
        <key=.p6ident> <.ws> '=>' <.ws> $<value>=[<cgroup>|<podint>|<podstr>]
    }

    token cgroup {
        | '[' ~ ']' [$<sbitem>=(<podint>|<podstr>) +% [<.ws> ',' <.ws>]]
        | '<' ~ '>' [$<qwitem>=([<![>]> \S]+) +% <.ws>]
        | '{' ~ '}' [$<cbitem>=(<config_option>) +% [<.ws> ',' <.ws>]]
    }

    # TO-CORE: podint will likely want to parse any <integer>, and podstr any
    # kind of Q lang (though variable interpolation is disallowed)
    token podint { \d+ }
    token podstr {
        | \' ~ \' (<-[\'\\]> | \\\\ | \\\')+
        | \" ~ \" (<-[\"\\]> | \\\\ | \\\")+
    }

    # this token lives to produce an error. Do not call unless/until you know
    # it's needed
    token non-const-term {
        | <?before <+[$@%&]> | "::"> (\S\S?! <.p6ident>) {$¢.typed_panic(X::Pod6::BadConfig, message => "Variable \"$0\" found in pod configuration; only constants are allowed")}
        | "#" <.typed_panic(X::Pod6::BadConfig, message => "Unexpected # in pod configuration. (Were you trying to comment out something?)")>
        | ([\S & <-[\])>]>]+) {$¢.typed_panic(X::Pod6::BadConfig, message => "Unknown term \"$0\" in configuration. Only constants are allowed.")}
    }

    token configset {
        <config_option> +%% [ <.ws>
                              [ $<badcomma>=[\,] <.ws>
                                {$<badcomma>[*-1].CURSOR.typed_worry(X::Pod6::BadConfig::Comma)}
                              ]?
                            ]
    }

    token extra_config_line {
        <.start_line> \= \h+
        <configset>
        <.end_line>
    }


    proto token pseudopara {*}
    multi token pseudopara:sym<implicit_code> {
        :my %*THIS_CONFIG := nqp::getlexdyn('%*THIS_CONFIG');

        <?{$*W.pod_imply_blocks}> (\h+)

        {
            self.prime_code_line(nqp::chars(~$0));
            %*THIS_CONFIG := $*W.pod_config_for_block("code");
        }

        <!before <.end_non_delim>> $<line>=(<one_token_text>+ <end_line>)
        [<!before <.end_non_delim>> <.start_code_line> $<line>=(<one_token_text>+ <end_line>)]*
    }

    multi token pseudopara:sym<implicit_para> {
        :my %*THIS_CONFIG := nqp::getlexdyn('%*THIS_CONFIG');

        <?{$*W.pod_imply_blocks}> <!before \h>

        { %*THIS_CONFIG := $*W.pod_config_for_block("para"); }

        <!before <.end_non_delim>> $<line>=(<one_token_text>+ <end_line>)
        [<!before <.end_non_delim>> <.start_line> $<line>=(<one_token_text>+ <end_line>)]*
    }

    multi token pseudopara:sym<nothing_implied> {
        <!{$*W.pod_imply_blocks}>

        <!before <.end_non_delim>> $<line>=(<one_token_text>+ <end_line>)
        [<!before <.end_non_delim>> <.start_line> $<line>=(<one_token_text>+ <end_line>)]*
    }

    token one_token_text {
        <formatting_code> || \N
    }

    token fcode_open {
        [
        | ('<'+) { $*OPENER := $0.Str; $*CLOSER := '>' x $0.Str.chars }
        | ('«'+) { $*OPENER := $0.Str; $*CLOSER := '»' x $0.Str.chars }
        ]
    }

    token fcode_scheme { <( [<![:]> <!before $*CLOSER> .]+ )> ':' }

    token fcode_inside {
        [ <!before $*CLOSER> [<?{$*PIPE_END}> <![|]> || <!{$*PIPE_END}>]
          [
          | [ <formatting_code> || <!before $*OPENER> $<one>=[\N] ]
          | <.end_line> <!blank_line> <.start_line>
          | $<one>=[$*OPENER]
            [
            | <?same> <.typed_panic(X::Pod6::FCode::TooManyAngles)>
            | { $*BALANCES++ } <fcode_inside>
            ]
          ]
        ]*

        [<?{$*BALANCES > 0}> $<one>=[$*CLOSER] { $*BALANCES-- }]?
    }

    token fcode_close {
        [
        | $*CLOSER
        | <.blank_line> <.typed_worry(X::Pod6::FCode::ForcedStop)>
        ]
    }

    proto token formatting_code {*}

    multi token formatting_code:sym<A> {
        :my %*THIS_CONFIG := nqp::getlexdyn('%*THIS_CONFIG');
        A

        <?{$*W.pod_allow_format_code('A')}>

        :my $*FC := 'A';
        :my $*OPENER;
        :my $*CLOSER;

        <.fcode_open>

        $<ident>=[ $<sigil>=[<[$@%&]>]? <.p6ident> ]

        # currently doesn't allow arguments to method
        $<methods>=(\. <mname=p6ident>)*

        <.fcode_close>
    }

    multi token formatting_code:sym<D> {
        :my %*THIS_CONFIG := nqp::getlexdyn('%*THIS_CONFIG');
        D

        <?{$*W.pod_allow_format_code('D')}>

        :my $*FC := 'D';

        :my $*OPENER;
        :my $*CLOSER;

        <.fcode_open>

        :my $*BALANCES = 0;
        :my $*PIPE_END := 1;

        [<display=fcode_inside> '|']?

        $<syn>=( [<![;]> <!before $*CLOSER> .]+ ) +% ';'

        <.fcode_close>
    }

    multi token formatting_code:sym<E> {
        :my %*THIS_CONFIG := nqp::getlexdyn('%*THIS_CONFIG');
        E

        <?{$*W.pod_allow_format_code('E')}>

        :my $*FC := 'E';

        :my $*OPENER;
        :my $*CLOSER;

        <.fcode_open>

        $<terms>=(
            <.ws>
            [
            | '0x' $<xnum>=[[_? <.xdigit>]+]
            | '0o' $<onum>=[[_? <:Numeric_Value(0..^8)>]+] # XXX use of ..^ is a workaround, should be 0..7
            | '0b' $<bnum>=[[_? <:Numeric_Value(0..^2)>]+] # XXX also, meant to use Nv instead of long name
            | '0d'? $<dnum>=[\d+]
            | $<uname>=[<.alpha> [ <[\ -]> <.alpha> | <.alnum> ]* ]
            ]
            <.ws>
        ) +% ';'

        <.fcode_close>
    }

    multi token formatting_code:sym<LP> {
        :my %*THIS_CONFIG := nqp::getlexdyn('%*THIS_CONFIG');
        $<fcode>=[<[LP]>]

        :my $*FC; {$*FC = ~$<fcode>}

        <?{$*W.pod_allow_format_code($*FC)}>

        :my $*OPENER;
        :my $*CLOSER;

        <.fcode_open>

        :my $*PIPE_END := 1;
        :my $*BALANCES = 0;

        [<display=fcode_inside> '|']?

        [ <fcode_scheme> | <?{$*FC eq 'L'}> <?before '#'> ]
        $<address>=[[<!before $*CLOSER> .]+]

        <.fcode_close>
    }

    multi token formatting_code:sym<M> {
        :my %*THIS_CONFIG := nqp::getlexdyn('%*THIS_CONFIG');
        M

        <?{$*W.pod_allow_format_code('M')}>

        :my $*FC := 'M';

        :my $*OPENER;
        :my $*CLOSER;
        :my $*BALANCES = 0;

        <.fcode_open>

        <fcode_scheme>
        <text=fcode_inside>

        <.fcode_close>
    }

    multi token formatting_code:sym<X> {
        :my %*THIS_CONFIG := nqp::getlexdyn('%*THIS_CONFIG');
        X

        <?{$*W.pod_allow_format_code('X')}>

        :my $*FC := 'X';

        :my $*OPENER;
        :my $*CLOSER;

        <.fcode_open>

        :my $*PIPE_END := 1;
        :my $*BALANCES = 0;

        [<display=fcode_inside> '|']?

        $<entry>=(
            $<main>=[[<![,;]> <!before $*CLOSER> .]+]
            [',' $<subent>=[[<![,;]> <!before $*CLOSER> .]+]]*
        ) +% ';'

        <.fcode_close>
    }

    multi token formatting_code:sym<normal> {
        :my %*THIS_CONFIG := nqp::getlexdyn('%*THIS_CONFIG');

        $<fcode>=[<[BCIKNRSTUVZ]>]

        :my $*FC; { $*FC := ~$<fcode> }

        <?{$*W.pod_allow_format_code($*FC)}>

        :my $*OPENER;
        :my $*CLOSER;
        :my $*BALANCES = 0;

        <.fcode_open>

        <contents=fcode_inside>

        <.fcode_close>
    }

    multi token formatting_code:sym<reserved> {
        $<fcode>=[<[FGHJOQWY]>] <?before <[<«]>>

        <?{$*W.pod_allow_format_code(~$<fcode>)}>

        {$¢.typed_sorry(X::Pod6::FormatCode::ReservedCode, culprit => ~$<fcode>)}
    }

    # TO-CORE use the actual rule instead of p6ident, the version that doesn't
    # allow ::
    token p6ident {
        [<.alpha>|_]
        [
        | <.alnum>
        | _
        | [\- | \'] [<.alpha>|_]
        ]*
    }

    # meant as a lookahead for when we need to know if a new block would be
    # starting at the current position
    token new_directive {
        \h* \= [
                 [
                   [begin|for|end] \h+
                 ]?
                 [<reserved_name> | <typename>]
               ]
    }
}