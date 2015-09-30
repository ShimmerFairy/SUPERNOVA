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
use GrammarError;

#use Grammar::Tracer;

grammar Pod6::Grammar does GramError {
    token TOP {
        :my @*POD_BLOCKS := nqp::list();

        :my $*LAST_BEGIN;

        :my $*VMARGIN;

        # TO-CORE stuff we won't need
        :my @*WORRIES;
        :my @*SORROWS;
        :my $*SORRY_LIMIT := 10;

        <.start_document>

        <.blank_line>*
        [<block>
         <.blank_line>*]*

        [ $ || <.panic(X::Pod6::Didn'tComplete)> ]

        <.cry-sorrows>
        <.express-worries>
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
        $*VMARGIN = numify-margin($margin);
        self;
    }

    method prime_code_line($margin) {
        my $size = numify-margin($margin);
        @*POD_BLOCKS[*-1].set-cvmargin($size);
        self;
    }

    token start_line {
        ^^ " " ** { @*POD_BLOCKS[*-1].get-margin() }
    }

    token start_code_line {
        ^^ " " ** { @*POD_BLOCKS[*-1].get-margin() + @*POD_BLOCKS[*-1].get-code-margin() }
    }

    token end_line {
        $$ [\n || $ || <.parse-fail("Unknown line ending found.")>]
    }

    token blank_line {
        ^^ \h* <.end_line>
    }

    token blank_or_eof {
        <.blank_line> || $
    }

    token end_non_delim { <.blank_or_eof> | <?before <.new_directive>> }

    token new_block { <?> }

    token parent_block { <?> }

    # high-level block handling

    token block {
        ^^ $<litmargin>=(\h*) <?before <new_directive>>
        {self.prime_line(~$<litmargin>)}

        <directive>
#        <.parent_block>
    }

    proto token directive {*}

    multi token directive:sym<delim> {
        "=begin" <.ws> # XXX want :: here
            <block_name> <.ws>
            :my $*BLOCK_NAME; {$*BLOCK_NAME := $<block_name>} <.new_block>

        <.block_config>

        $<contents>=( <!before \h* "=end">
          [
          | <block>
          | <.start_line> <pseudopara>
          | <.blank_line>
          ]
        )*

        <.start_line> "=end" <.ws> [$<block_name>
                                   || <badname=.block_name> {$<badname>.CURSOR.panic(X::Pod6::MismatchedEnd, HINT-MATCH => $/)}
                                   ] <.ws> <.end_line>
        {$*LAST_BEGIN = $/} # for hints on extraneous =end blocks
    }

    multi token directive:sym<para> {
        "=for" <.ws> # XXX want :: here
            <block_name> <.ws>
            :my $*BLOCK_NAME; {$*BLOCK_NAME := $<block_name>} <.new_block>

        <.block_config>

        <.start_line> <pseudopara>

        <.end_non_delim>
    }

    multi token directive:sym<abbr> {
        \= <!not_name> <block_name> <.ws>
            :my $*BLOCK_NAME; {$*BLOCK_NAME := $<block_name>} <.new_block>

        # implied code blocks can only start on the next line, since only there
        # can we check for indentation. However, since we eat up all the
        # whitespace on the first line (see above <.ws>), we don't need to do
        # anything special.

        [<.end_line> <.start_line>]? <pseudopara>

        <.end_non_delim>
    }

    multi token directive:sym<encoding> {
        "=encoding" <.ws> #`(::)
        :my $*BLOCK_NAME; {$*BLOCK_NAME := shim-unbox_s("encoding")} <.new_block>
        $<encoding>=[\N+ <.end_line>
            [<!blank_or_eof> <.start_line> \N+ <.end_line>]*]

        <.end_non_delim>

        {$¢.worry(X::Pod6::Encoding, target-enc => ~$<encoding>)}
    }

    multi token directive:sym<alias> {
        "=alias" <.ws> $<AVal=.p6ident> <.ws>
        :my $*BLOCK_NAME; {$*BLOCK_NAME := shim-unbox_s("alias")} <.new_block>
        [<.end_line> {$¢.panic(X::Pod6::Alias, atype => "Contextual")}]?

        \N+ {$¢.sorry(X::Pod6::Alias, atype => "Macro")}
        <.end_line> <.end_non_delim>
    }

    multi token directive:sym<config> {
        "=config" <.ws> #`(::)
        :my $*BLOCK_NAME; {$*BLOCK_NAME := shim-unbox_s("config")} <.new_block>
        $<thing>=[<.block_name>|<[A..Z]> "<>"] <.ws>
        <configset> <.end_line>
        <extra_config_line>*
        <.end_non_delim>
    }

    multi token directive:sym<end> {
        "=end" <.ws> <block_name> #`(::)
        {
            if $*LAST_BEGIN {
                $¢.panic(X::Pod6::ExtraEnd, HINT-MATCH => $*LAST_BEGIN);
            } else {
                $¢.panic(X::Pod6::ExtraEnd);
            }
        }
    }

    token block_name {
        || [<standard_name> | <semantic_standard_name>]
        || <not_name> { $¢.panic(X::Pod6::Block::DirectiveAsName, culprit => ~$<not_name>) }
        || <reserved_name> { $¢.sorry(X::Pod6::Block::ReservedName, culprit => ~$<reserved_name>) }
        || <typename>
    }

    token standard_name {
        | code
        | comment
        | data
        | defn
        | finish
        | head $<level>=[\d+]
        | input $<level>=[\d+]?
        | item
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
        | ACKNOWLEDGMENTS?
        | COPYRIGHTS?
        | DISCLAIMERS?
        | LICENCES?
        | LICENSES?
        | TITLES?
        | SECTIONS?
        | CHAPTERS?
        | APPENDI[X|CES]
        | TOCS?
        | IND[EX|ICES]
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

    token reserved_name { [<:Lower>+ | <:Upper>+] <!ww> }

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
        | <?[$@%&]> <.panic(X::Pod6::BadConfig, message => "Attempted to use variable as colonpair; only constants are allowed in Pod configuration")>
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
        | <?before <+[$@%&]> | "::"> (\S\S?! <.p6ident>) {$¢.panic(X::Pod6::BadConfig, message => "Variable \"$0\" found in pod configuration; only constants are allowed")}
        | "#" <.panic(X::Pod6::BadConfig, message => "Unexpected # in pod configuration. (Were you trying to comment out something?)")>
        | ([\S & <-[\])>]>]+) {$¢.panic(X::Pod6::BadConfig, message => "Unknown term \"$0\" in configuration. Only constants are allowed.")}
    }

    token configset {
        <config_option> +%% [ <.ws>
                              [ $<badcomma>=[\,] <.ws>
                                {$<badcomma>[*-1].CURSOR.worry(X::Pod6::BadConfig::Comma)}
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
        <?{@*POD_BLOCKS[*-1].implies-code()}> (\h+) # probably want ::

        {self.prime_code_line(~$0)}


        <!before <.end_non_delim>> $<line>=(<one_token_text>+ <end_line>)
        [<!before <.end_non_delim>> <.start_code_line> $<line>=(<one_token_text>+ <end_line>)]*
    }

    multi token pseudopara:sym<implicit_para> {
        <?{@*POD_BLOCKS[*-1].implies-para()}>

        # we can accept indented stuff, _if_ code blocks can't be implicit here
        [<!{@*POD_BLOCKS[*-1].implies-code()}> \h* | <!before \h>]

        # probably want ::

        <!before <.end_non_delim>> $<line>=(<one_token_text>+ <end_line>)
        [<!before <.end_non_delim>> <.start_line> $<line>=(<one_token_text>+ <end_line>)]*
    }

    multi token pseudopara:sym<nothing_implied> {
        # probably not needed when :: used on the other multis
        <!{@*POD_BLOCKS[*-1].implies-code() || @*POD_BLOCKS[*-1].implies-para()}>

        <!before <.end_non_delim>> $<line>=(<one_token_text>+ <end_line>)
        [<!before <.end_non_delim>> <.start_line> $<line>=(<one_token_text>+ <end_line>)]*
    }

    token one_token_text {
        [<!formatting_code> \N]+ | <formatting_code>
    }

    token formatting_code { <!> }

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