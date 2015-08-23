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

#use Grammar::Tracer;

use Exception;

role GramError {
    has $!nl-list;

    # like HLL::Compiler.lineof, but gives a column too
    method linecol($text, $at) {
        my $tsize := nqp::chars($text);
        my $lookfrom := 0;
        my $line := 0;

        my $pos := nqp::list_i();

        unless $!nl-list {
            $!nl-list := nqp::list_i();

            my sub nextnl {
                nqp::findcclass(nqp::const::CCLASS_NEWLINE, $text, $lookfrom, $tsize);
            }

            while nqp::islt_i(($lookfrom := nextnl()), $tsize) {
                # don't count \r\n as two newlines
                if nqp::eqat($text, "\r\n", $lookfrom) {
                    $lookfrom := nqp::add_i($lookfrom, 2);
                } else {
                    $lookfrom := nqp::add_i($lookfrom, 1);
                }

                # unlike HLL::Compiler's lineof, we count \r\n as ending after
                # the \n. This opens up the chance that we'll get a pointer in
                # the middle (\r⏏\n) identifying itself on the line \r\n ends
                # (whereas lineof would report for the next line), but it's
                # generally not a good idea to wind up there anyway ☺
                nqp::push_i($!nl-list, $lookfrom);
            }
        }

        # now to find the right line number

        my $lo := 0;
        my $hi := nqp::elems($!nl-list);
        my $linefind;

        while nqp::islt_i($lo, $hi) {
            $linefind := nqp::div_i(nqp::add_i($lo, $hi), 2);
            if nqp::isgt_i(nqp::atpos_i($!nl-list, $linefind), $at) {
                $hi := $linefind;
            } else {
                $lo := nqp::add_i($linefind, 1);
            }
        }

        nqp::bindpos_i($pos, 0, nqp::add_i($lo, 1));

        # now to get the column
        if $lo == 0 {
            nqp::bindpos_i($pos, 1, $at);
        } else {
            nqp::bindpos_i($pos, 1, nqp::sub_i($at, nqp::atpos_i($!nl-list, nqp::sub_i($lo, 1))));
        }

        $pos;
    }

    method takeline(str $fromthis, int $lineno) {
        my $arrline := nqp::sub_i($lineno, 1);

        my $startpos;
        my $endpos;
        if $arrline == 0 {
            $startpos := 0;
        } else {
            $startpos := nqp::atpos_i($!nl-list, nqp::sub_i($arrline, 1));
        }

        if $arrline == nqp::elems($!nl-list) {
            $endpos := nqp::chars($fromthis);
        } else {
            $endpos := nqp::atpos_i($!nl-list, $arrline);
        }

        nqp::substr($fromthis, $startpos, nqp::sub_i($endpos, $startpos));
    }

    # XXX Use ANSIColor
    # simple parse failure
    method parse-fail($msg = "No message was given") {
        $/ = self.MATCH();

        my $string := shim-unbox_s($/.orig);

        my $failat := self.linecol($string, shim-unbox_i($/.from));

        my $fline := nqp::atpos_i($failat, 0);
        my $fcol  := nqp::atpos_i($failat, 1);

        my $errorline := nqp::atpos(nqp::split("\n", $string), $fline - 1);
        note "\e[41;1m===SORRY!===\e[0m Error in parsing file:";
        note $msg;
        note "In file at line ", $fline, ", col ", $fcol, ":";
        note "\e[32m", nqp::substr($errorline, 0, $fcol),
             "\e[33m⏏",
             "\e[31m", nqp::substr($errorline, $fcol),
             "\e[0m";

        exit(1);
    }

    method make-ex($/, Exception $type, %opts is copy) {
        my $linecol := self.linecol(shim-unbox_s($/.orig), shim-unbox_i($/.from));

        my $fled-line := self.takeline(shim-unbox_s($/.orig), nqp::atpos_i($linecol, 0));

        %opts<goodpart> := nqp::substr($fled-line, 0, nqp::atpos_i($linecol, 1));
        %opts<badpart>  := nqp::substr($fled-line, nqp::atpos_i($linecol, 1));

        %opts<err-flc> := X::FLC.new(file => $*FILENAME,
                                         line => shim-box_i(nqp::atpos_i($linecol, 0)),
                                         col  => shim-box_i(nqp::atpos_i($linecol, 1)));

        if %opts<HINT-MATCH>:exists {
            my $hint := %opts<HINT-MATCH>;

            my $hintlc := self.linecol(shim-unbox_s($hint.orig), shim-unbox_i($hint.from));

            my $hint-line := self.takeline(shim-unbox_s($hint.orig), nqp::atpos_i($hintlc, 0));

            %opts<hint-beforepoint> := nqp::substr($hint-line, 0, nqp::atpos_i($hintlc, 1));
            %opts<hint-afterpoint>  := nqp::substr($hint-line, nqp::atpos_i($hintlc, 1));

            %opts<hint-flc> := X::FLC.new(file => $*FILENAME,
                                              line => shim-box_i(nqp::atpos_i($hintlc, 0)),
                                              col  => shim-box_i(nqp::atpos_i($hintlc, 1)));

            %opts<HINT-MATCH>:delete;
        }

        $type.new(|%opts);
    }

    # stuff that's potentially troublesome, but doesn't prevent a useful result
    # from the parser
    method worry(Exception $type, *%exnameds) {
        @*WORRIES.push(self.make-ex(self.MATCH, $type, %exnameds));
        self;
    }

    # stuff that leaves the parser unable to return something useful, but
    # doesn't prevent the parser from continuing (in order to find more
    # sorrows/panics)
    method sorry(Exception $type, *%exnameds) {
        @*SORROWS.push(self.make-ex(self.MATCH, $type, %exnameds));

        if +@*SORROWS >= $*SORRY_LIMIT { # we've got too much to be sorry for, bail
            self.give-up-ghost();
        }
        self;
    }

    # curse of fatal death --- there's no way the parser can even parse more
    # stuff after something like this
    method panic(Exception $type, *%exnameds) {
        my $ex := self.make-ex(self.MATCH, $type, %exnameds);
        if +@*SORROWS || +@*WORRIES {
            self.give-up-ghost($ex);
        } else {
            $ex.throw;
        }
        self;
    }

    method cry-sorrows() {
        if +@*SORROWS == 1 && !+@*WORRIES {
            @*SORROWS[0].throw;
        } elsif @*SORROWS > 1 {
            self.give-up-ghost();
        }
        self;
    }

    method express-worries() {
        return self unless +@*WORRIES;
        if +@*SORROWS {
            warn "Issue in Grammar Error Reporter: worries being expressed before sorrows cried";
        }

        self.give-up-ghost();
        self;
    }

    method give-up-ghost(X::Pod6 $panic?) {
        my $ghost;
        with $panic {
            $ghost = X::Epitaph.new(:$panic, worries => @*WORRIES, sorrows => @*SORROWS);
        } else {
            $ghost = X::Epitaph.new(worries => @*WORRIES, sorrows => @*SORROWS);
        }

        if +@*SORROWS {
            $ghost.throw
        } else {
            print($ghost.gist)
        }
    }
}

# TO-CORE not sure if having this class would be good in core
class FCAllow {
    # TO-CORE should be %
    has $!status;

    submethod BUILD {
        $!status := nqp::hash();
    }

    method !populate(int $val) {
        my $ord := nqp::ord(shim-unbox_s("A"));
        my $loop := shim-unbox_i(26);

        nqp::while(nqp::isgt_i($loop, 0),
            nqp::stmts(
                nqp::bindkey($!status, nqp::chr($ord), $val),
                nqp::bind($ord, nqp::add_i($ord, 1)),
                nqp::bind($loop, nqp::sub_i($loop, 1))));
    }

    method !flip(str $ord, int $to) {
        if nqp::existskey($!status, $ord) {
            nqp::bindkey($!status, $ord, $to)
        } else {
            die "OH NO! {chr($ord)} !";
        }
    }

    multi method permit() {
        self!populate(shim-unbox_i(1));
        self;
    }

    multi method permit(*@codes) {
        for @codes {
            self!flip(shim-unbox_s($_), 1);
        }
        self;
    }

    multi method revoke() {
        self!populate(shim-unbox_i(0));
        self;
    }

    multi method revoke(*@codes) {
        for @codes {
            self!flip(shim-unbox_s($_), 1);
        }
        self;
    }

    method allowed($code) {
        nqp::defor(nqp::atkey($!status, shim-unbox_s($code)), 0);
    }
}

grammar Pod6::Grammar does GramError {
    token TOP {
        :my @*V_MARGINS := nqp::list();
        :my $*CAN_CODE;
        :my $*CAN_PARA;
        :my $*FC_BLANKSTOP;
        :my @*FC_ALLOWED := nqp::list();

        # TO-CORE stuff we won't need
        :my @*WORRIES;
        :my @*SORROWS;
        :my $*SORRY_LIMIT := 10;

        <.blank_line>*
        [<block>
         <.blank_line>*]+

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
    method prime_line($margin) {
        my $de-tabbed := nqp::split("\t", shim-unbox_s($margin));

        if nqp::elems($de-tabbed) == 1 {
            nqp::push(@*V_MARGINS, nqp::chars($margin));
        } else {
            nqp::push(
                @*V_MARGINS,
                nqp::chars(
                    nqp::join(
                        nqp::x(
                            " ",
                            ($?TABSTOP // 8)),
                        $de-tabbed)));
        }

        self;
    }

    method unprime_line {
        nqp::pop(@*V_MARGINS);
        self;
    }

    token start_line {
        ^^    " " ** {@*V_MARGINS[*-1]}
    }

    token end_line {
        $$ [\n || $ || <.parse-fail("Unknown line ending found.")>]
    }

    token blank_line {
        ^^ \h* <.end_line>
    }

    # FC allowance methods

    method now-permit-all() {
        nqp::push(@*FC_ALLOWED, FCAllow.new.permit);
        self;
    }

    method now-revoke-all() {
        nqp::push(@*FC_ALLOWED, FCAllow.new.revoke);
        self;
    }

    method fc-cando($item) {
        nqp::atpos(@*FC_ALLOWED, -1).allowed($item);
    }

    method fc-pop() {
        nqp::pop(@*FC_ALLOWED);
        self;
    }

    # high-level block handling

    token block {
        ^^ $<litmargin>=(\h*) <?before <new_directive>>
        {self.prime_line(~$<litmargin>)}

        <directive>

        <.unprime_line> <.fc-pop>
    }

    proto token directive {*}

    multi token directive:sym<delim> {
        "=begin" <.ws> # XXX want :: here
            <block_name> <.ws>
            <configopt> *%% <.ws> <.end_line>

        <extra_config_line>*

        {$*FC_BLANKSTOP := 0}

        [ <!before \h* "=end">
          [
          | <block>
          | <.start_line> <pseudopara> <.blank_line>?
          ]
        ]*

        <.start_line> "=end" <.ws> [$<block_name>
                                   || <badname=.block_name> {$<badname>.CURSOR.panic(X::Pod6::MismatchedEnd, HINT-MATCH => $/)}
                                   ] <.ws> <.end_line>
    }

    multi token directive:sym<para> {
        "=for" <.ws> # XXX want :: here
            <block_name> <.ws>
            <configopt> *%% <.ws> <.end_line>

        <extra_config_line>*

        {$*FC_BLANKSTOP := 1}

        <.start_line> <pseudopara>

        <.blank_line>
    }

    multi token directive:sym<abbr> {
        \= <block_name> <.ws>

        # Implicit code blocks only work if started on the next line, the other
        # possibilities can start on the same line.

        {$*FC_BLANKSTOP := 1}

        [
        | <.end_line> <.start_line> <pseudopara>
        # XXX hackish way to disallow code blocks
        | :my $*CAN_CODE := 0; <pseudopara>
        ]

        <.blank_line>
    }

    token block_name {
        || [<standard_name> | <semantic_standard_name>]
        || <not_name> { $¢.sorry(X::Pod6::Block::DirectiveAsName, culprit => ~$<not_name>) }
        || <reserved_name> { $¢.sorry(X::Pod6::Block::ReservedName, culprit => ~$<reserved_name>) }
        || <typename>
    }

    token standard_name {
        | code                        { $*CAN_CODE := 1 } <.now-revoke-all>
        | comment                                         <.now-permit-all>
        | data                                            <.now-permit-all>
        | defn    { $*CAN_PARA := 1 }                     <.now-permit-all>
        | finish  { $*CAN_PARA := 1 }                     <.now-permit-all>
        | head                                            <.now-permit-all>
        | input                                           <.now-permit-all>
        | item    { $*CAN_PARA := 1 } { $*CAN_CODE := 1 } <.now-permit-all>
        | nested  { $*CAN_PARA := 1 } { $*CAN_CODE := 1 } <.now-permit-all>
        | output                                          <.now-permit-all>
        | para                                            <.now-permit-all>
        | pod     { $*CAN_PARA := 1 } { $*CAN_CODE := 1 } <.now-permit-all>
        | table                                           <.now-permit-all>
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
        ] { $*CAN_PARA := 1; $*CAN_CODE := 1; self.now-permit-all }
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
        | <:Upper>+ <:Lower> [<:Upper>|<:Lower>]*
        | <:Lower>+ <:Upper> [<:Lower>|<:Upper>]*
    }

    # unfortunately, even after transferring to CORE, we'll need our own rule to
    # handle config options, because we can only accept constant keys and
    # values, but Perl6::Grammar's parser allow non-constants
    token configopt {
        :my $*ADV_BINARY := -1;
        [
        | \: [\! {$*ADV_BINARY := 0}]?
          [$<ckey>=[<.ident> +% \-] || <.non-const-term>]
          $<cvalue>=[
            | [
              | <podassociative>
              | <podpositional>
              | \< ~ \> $<podqw>=[ [[<!before \h | <?[>]>> .]+] +%% <.ws> ]
              | \( ~ \) [[<podstr>|<podint>]||<.non-const-term>]
              ] [ <?{$*ADV_BINARY == 0}> {$¢.panic(X::Pod6::BadConfig, message => "Cannot negate adverb and provide value ~$<cvalue>")} ]
            | {unless $*ADV_BINARY == 0 { $*ADV_BINARY := 1} }
              [ <!before \s> $<badtext>=[[\S & <-[,]>]+] {$<badtext>.CURSOR.panic(X::Pod6::BadConfig, message => "Unknown text \"$0\" after key")} ]?
          ]
        | [$<ckey>=[<.ident> +% \-] || <.non-const-term>]
          <.ws> ["=>" || {$¢.panic(X::Pod6::BadConfig, message => "Bad key; expecting => after key or : before it")}]
          <.ws>
          $<cvalue>=[
              [
              | <podint>
              | <podstr>
              | <podpositional>
              | <podassociative>
              ] || <.non-const-term>
                || {$¢.panic(X::Pod6::BadConfig, message => "No value found after fatarrow; please use :$<ckey> if you meant to set a binary flag")}
          ]
        ]
    }

    # TO-CORE: podint will likely want to parse any <integer>, and podstr any
    # kind of Q lang (though variable interpolation is disallowed)
    token podint { \d+ }
    token podstr {
        | \' ~ \' [<-['\\]> | \\\\ | \\\']+
        | \" ~ \" [<-["\\]> | \\\\ | \\\"]+
    }

    token podpositional {
        \[ ~ \] [[[<podint>|<podstr>]||<.non-const-term>] +% [<.ws> \, <.ws>]]
    }

    token podassociative {
        \{ ~ \} [<configopt> +% [<.ws> \, <.ws>]]
    }

    # this token lives to produce an error. Do not call unless/until you know
    # it's needed
    token non-const-term {
        | <?before <+[$@%&]> | "::"> (\S\S?! <.ident>) {$¢.panic(X::Pod6::BadConfig, message => "Variable \"$0\" found in pod configuration; only constants are allowed")}
        | "#" <.panic(X::Pod6::BadConfig, message => "Unexpected # in pod configuration. (Were you trying to comment out something?)")>
        | ([\S & <-[\])>]>]+) {$¢.panic(X::Pod6::BadConfig, message => "Unknown term \"$0\" in configuration. Only constants are allowed.")}
    }

    token extra_config_line {
        <.start_line> \= \h+
        <configopt> +%% [<.ws> [$<badcomma>=[\,] <.ws> {$<badcomma>[*-1].CURSOR.worry(X::Pod6::BadConfig::Comma)}]?]
        <.end_line>
    }


    proto token pseudopara {*}
    multi token pseudopara:sym<implicit_code> {
        <?{$*CAN_CODE}> # probably want ::
        # TOCORE *-1 -> -1
        (\h+) {self.prime_line(~$0); nqp::push(@*V_MARGINS, nqp::add_i(nqp::pop(@*V_MARGINS), @*V_MARGINS[*-1]))}

        :my $*FC_BLANKSTOP := 1;  # implied code block overrides existing FC setting
        <.now-revoke-all>         # don't allow formatting codes by default

        <!before <new_directive>> $<line>=(<one_token_text>+ <.end_line>)
        [<!blank_line> <.start_line> <!before <new_directive>> $<line>=(<one_token_text>+ <.end_line>)]*

        <.unprime_line> <.fc-pop>
    }

    multi token pseudopara:sym<implicit_para> {
        <?{$*CAN_PARA}> # probably want ::

        # we can accept indented stuff, _if_ code blocks can't be implicit here
        [<!{$*CAN_CODE}> \h*]?

        :my $*FC_BLANKSTOP := 1; # implied para overrides existing FC setting

        <!before <new_directive>> <one_token_text>+ <.end_line>
        [<!blank_line> <.start_line> <!before <new_directive>> <one_token_text>+ <.end_line>]*
    }

    multi token pseudopara:sym<nothing_implied> {
        <!{$*CAN_PARA}> <!{$*CAN_CODE}> # probably not needed when :: used on the other multis
        <!before <new_directive>> <one_token_text>+ <.end_line>
        [<!blank_line> <.start_line> <!before <new_directive>> <one_token_text>+ <.end_line>]*
    }

    token one_token_text {
        \N | <format_code>
    }

    token format_code {
        :my $endcond;
        :my $opener;
        :my $closeleft := shim-unbox_i(1);
        $<which>=[ <[A..Z]> ] <?{$¢.fc-cando(~$<which>)}>
        [
        | $<restart>=(\<+) {$opener := "<"; $endcond := ">" x (~$<restart>).chars}
        | $<restart>=(\«+) {$opener := "«"; $endcond := "»" x (~$<restart>).chars}
        ]

        [ <?{$closeleft > 0}>
          [
          | $<restart>
            [
            | $opener+ { $¢.panic(X::Pod6::FCode::TooManyAngles) }
            | { $closeleft := nqp::add_i($closeleft, 1) }
            ]
          | <format_code>
          | $endcond { $closeleft := nqp::sub_i($closeleft, 1) }
          | <.end_line>
            [
            | [ <.new_directive> | <?{$*FC_BLANKSTOP}> <?blank_line> ]
              <.worry(X::Pod6::FCode::ForcedStop)>
              {$closeleft := 0}
            | <.start_line> .
            ]
          | <!before $endcond> \N
          ]
        ]+
    }

    # meant as a lookahead for when we need to know if a new block would be starting at the current position
    token new_directive {
        \h* \= [[[begin|for|end] \h+]? [<reserved_name> | <typename>]]
    }
}

# TEMP TEST

my $*FILENAME = "<internal-test>";

my $testpod = q:to/NOTPOD/;
    =begin pod :!autotoc
    =       :autotoc :imconfused :!ok
    Hello there L<ALL OK >
    Everybody

    Another para

        AND SOME CODE C««
            GLORIOUS»» CODE
        HELLO SAILOR

    And one more para
        Hanging indent!!~~
    =end pod
NOTPOD

Pod6::Grammar.parse($testpod);

for @<block>[0]<directive><pseudopara>[0]<one_token_text> {
    say ~$_;
}

for @<block>[0]<directive><pseudopara>[2]<line>[0..*-1] {
    for $_<one_token_text> {
        say ~$_
    }
}