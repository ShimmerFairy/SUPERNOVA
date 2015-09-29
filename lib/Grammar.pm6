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

# TO-CORE nqp-ify class
class PodScope {
    has $!fc-need-allow = False;
    has @!fc-allowed;
    has $!fc-blankstop = True;
    has $!vmargin = 0;
    has $!implied-para = False;
    has $!implied-code = False;

    has $!locked-scope = False; # for things like =config, to avoid other rules changing stuff

    has %!config;

    has $!implied-para-mode = False;
    has $!implied-code-mode = False;
    has $!imp-code-vmargin = 0;

    method enter-para { $!implied-code-mode || !$!implied-para ?? False !! ($!implied-para-mode = True) }
    method exit-para  { $!implied-para-mode = False }
    method enter-code { $!implied-para-mode || !$!implied-code ?? False !! ($!implied-code-mode = True) }
    method exit-code  { $!implied-code-mode = False }

    method set-this-config(Str $key, Str $value) {
        # make sure the key used here isn't a valid P6 identifier, to avoid any
        # conflicts with block names
        %!config<-THIS->{$key} = $value;
    }

    method set-config-for(Str $block, Str $key, Str $value) {
        %!config{$block}{$key} = $value;
    }

    method get-this-config(Str $key) { %!config<-THIS->{$key} }
    method get-config-for(Str $block, Str $key) { %!config{$block}{$key} }

    method lock-scope   { $!locked-scope = True  }
    method unlock-scope { $!locked-scope = False }

    method blanks-stop-fc($a) { unless $!locked-scope { $!fc-blankstop = ?$a } }
    method fc-stop-at-blank   { $!fc-blankstop }

    method disable-fc { unless $!locked-scope { $!fc-need-allow = True } }
    method allow-fc($fc) {
        unless $!locked-scope {
            die "wrong fc $fc" unless $fc ~~ "A".."Z";
            @!fc-allowed.push($fc)
        }
    }
    method can-fc($fc) {
        if $!fc-need-allow || $!implied-code-mode {
            $fc ~~ @!fc-allowed;
        } else {
            $fc ~~ "A".."Z"
        }
    }

    method imply-para { unless $!locked-scope { $!implied-para = True } }
    method imply-code { unless $!locked-scope { $!implied-code = True } }
    method unimply-para { unless $!locked-scope { $!implied-para = False } }
    method unimply-code { unless $!locked-scope { $!implied-code = False } }
    method can-para { $!implied-para }
    method can-code { $!implied-code }

    multi method set-margin(Int $wsnum, :$code) {
        unless $!locked-scope {
            if $code {
                $!imp-code-vmargin = $wsnum;
            } else {
                $!vmargin = $wsnum;
            }
        }
    }
    multi method set-margin(Str $char, :$code) {
        unless $!locked-scope {
            if $code {
                $!imp-code-vmargin = $char.substr(0, 1);
            } else {
                $!vmargin = $char.substr(0, 1);
            }
        }
    }
    method margin-is-char(:$code) { $code ?? $!imp-code-vmargin ~~ Str !! $!vmargin ~~ Str }
    method margin-is-size(:$code) { $code ?? $!imp-code-vmargin ~~ Int !! $!vmargin ~~ Int }
    method get-margin(:$code) { $code ?? $!imp-code-vmargin !! $!vmargin }
}

grammar Pod6::Grammar does GramError {
    token TOP {
        :my @*POD_SCOPES := nqp::list();

        :my $*LAST_BEGIN;

        # TO-CORE stuff we won't need
        :my @*WORRIES;
        :my @*SORROWS;
        :my $*SORRY_LIMIT := 10;

        <.blank_line>*
        [<block>
         <.blank_line>*]*

        [ $ || <.panic(X::Pod6::Didn'tComplete)> ]

        <.cry-sorrows>
        <.express-worries>
    }

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
        my $size = numify-margin($margin);
        @*POD_SCOPES[*-1].set-margin($size);
        self;
    }

    method prime_code_line($margin) {
        my $size = numify-margin($margin);
        @*POD_SCOPES[*-1].set-margin($size, :code);
        self;
    }

    token start_line {
        ^^ " " ** { @*POD_SCOPES[*-1].get-margin() }
    }

    token up_to_code { # for handling the extra indentation on implied code lines
        " " ** { @*POD_SCOPES[*-1].get-margin(:code) }
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

    # @*POD_SCOPES handlers

    method enter_scope {
        nqp::push(@*POD_SCOPES, PodScope.new);
        self;
    }

    method exit_scope {
        nqp::pop(@*POD_SCOPES);
        self;
    }

    method lock_scope {
        @*POD_SCOPES[*-1].lock-scope();
        self;
    }

    method unlock_scope {
        @*POD_SCOPES[*-1].unlock-scope();
        self;
    }

    method enter_para {
        @*POD_SCOPES[*-1].enter-para();
        self;
    }

    method enter_code {
        @*POD_SCOPES[*-1].enter-code();
        self;
    }

    method exit_para {
        @*POD_SCOPES[*-1].exit-para();
        self;
    }

    method exit_code {
        @*POD_SCOPES[*-1].exit-code();
        self;
    }

    # high-level block handling

    token block {
        ^^ $<litmargin>=(\h*) <?before <new_directive>>
        <.enter_scope>
        {self.prime_line(~$<litmargin>)}

        <directive>
        <.exit_scope>
    }

    proto token directive {*}

    multi token directive:sym<delim> {
        "=begin" <.ws> # XXX want :: here
            <block_name> <.ws>
            <configset>? <.end_line>

        <extra_config_line>*

        {@*POD_SCOPES[*-1].blanks-stop-fc(0)}

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
            <configset>? <.end_line>

        <extra_config_line>*

        {@*POD_SCOPES[*-1].blanks-stop-fc(1)}

        <.start_line> <pseudopara>

        <.end_non_delim>
    }

    multi token directive:sym<abbr> {
        \= <!not_name> <block_name> <.ws>

        {@*POD_SCOPES[*-1].blanks-stop-fc(1)}

        # implied code blocks can only start on the next line, since only there
        # can we check for indentation. However, since we eat up all the
        # whitespace on the first line (see above <.ws>), we don't need to do
        # anything special.

        [<.end_line> <.start_line>]? <pseudopara>

        <.end_non_delim>
    }

    multi token directive:sym<encoding> {
        "=encoding" <.ws> # ::
        $<encoding>=[\N+ <.end_line>
            [<!blank_or_eof> <.start_line> \N+ <.end_line>]*]

        <.end_non_delim>

        {$¢.worry(X::Pod6::Encoding, target-enc => ~$<encoding>)}
    }

    multi token directive:sym<alias> {
        "=alias" <.ws> $<AVal=.p6ident> <.ws>
        [<.end_line> {$¢.panic(X::Pod6::Alias, atype => "Contextual")}]?

        \N+ {$¢.sorry(X::Pod6::Alias, atype => "Macro")}
        <.end_line> <.end_non_delim>
    }

    multi token directive:sym<config> {
        "=config" <.ws> #`(::) <.lock_scope>
        $<thing>=[<.block_name>|<[A..Z]> "<>"] <.ws>
        <configset> <.end_line>
        <extra_config_line>*
        <.end_non_delim>
        <.unlock_scope>
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
        | code                                       { @*POD_SCOPES[*-1].imply-code() } { @*POD_SCOPES[*-1].disable-fc() }
        | comment
        | data
        | defn    { @*POD_SCOPES[*-1].imply-para() }
        | finish  { @*POD_SCOPES[*-1].imply-para() }
        | head $<level>=[\d+]
        | input $<level>=[\d+]?
        | item    { @*POD_SCOPES[*-1].imply-para() } { @*POD_SCOPES[*-1].imply-code() }
        | nested  { @*POD_SCOPES[*-1].imply-para() } { @*POD_SCOPES[*-1].imply-code() }
        | output
        | para
        | pod     { @*POD_SCOPES[*-1].imply-para() } { @*POD_SCOPES[*-1].imply-code() }
        | table
    }

    # since S26 states that each name and its plural is reserved, I decided to
    # pluralize even those that don't have a grammatical plural :P
    token semantic_standard_name {
        [
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
        ] { @*POD_SCOPES[*-1].imply-para(); @*POD_SCOPES[*-1].imply-code() }
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

    proto token config_option {*}

    multi token config_option:sym<colonpair> {
        ':'
        [
        | $<neg>=['!'] <key=.p6ident>
        | <?[$@%&]> <.panic(X::Pod6::BadConfig, message => "Attempted to use variable as colonpair; only constants are allowed in Pod configuration")>
        | <key=.p6ident> $<value>=['(' ~ ')' [<podint>|<podstr>] | <cgroup>]?
        ]
    }

    multi token config_option:sym<fatarrow> {
        <key=.p6ident> <.ws> '=>' <.ws> [<cgroup>|<podint>|<podstr>]
    }

    token cgroup {
        | '[' ~ ']' [$<sbitem>=(<podint>|<podstr>) +% [<.ws> ',' <.ws>]]
        | '<' ~ '>' [$<qwitem>=([<![>]> <!ws> .]+) +% <.ws>]
        | '{' ~ '}' [$<cbitem>=(<config_option>) +% [<.ws> ',' <.ws>]]
    }

    # TO-CORE: podint will likely want to parse any <integer>, and podstr any
    # kind of Q lang (though variable interpolation is disallowed)
    token podint { \d+ }
    token podstr {
        | \' ~ \' [<-[\'\\]> | \\\\ | \\\']+
        | \" ~ \" [<-[\"\\]> | \\\\ | \\\"]+
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
        <?{@*POD_SCOPES[*-1].can-code()}> (\h+) # probably want ::

        <.enter_code>
        {self.prime_code_line(~$0)}

        <!before <new_directive>> $<line>=(<one_token_text>+ <.end_line>)
        [<!blank_or_eof> <.start_line> <.up_to_code> <!before <new_directive>> $<line>=(<one_token_text>+ <.end_line>)]*
        <.exit_code>
    }

    multi token pseudopara:sym<implicit_para> {
        <?{@*POD_SCOPES[*-1].can-para()}> # probably want ::

        # we can accept indented stuff, _if_ code blocks can't be implicit here
        [<!{@*POD_SCOPES[*-1].can-code()}> \h*]?

        <.enter_para>

        <!before <new_directive>> $<line>=(<one_token_text>+ <.end_line>)
        [<!blank_or_eof> <.start_line> <!before <.new_directive>> $<line>=(<one_token_text>+ <.end_line>)]*

        <.exit_para>
    }

    multi token pseudopara:sym<nothing_implied> {
        <!{@*POD_SCOPES[*-1].can-code() || @*POD_SCOPES[*-1].can-para()}> # probably not needed when :: used on the other multis
        <!before <new_directive>> $<line>=(<one_token_text>+ <.end_line>)
        [<!blank_or_eof> <.start_line> <!before <new_directive>> $<line>=(<one_token_text>+ <.end_line>)]*
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