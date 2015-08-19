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

#use Grammar::Tracer;

role GramError {
    # basically reimpl of HLL::Compiler.lineof, except because we don't
    # (currently) save a list of positions, we can just wait until we hit
    # against a larger position than desired.

    method linecol($text, $at) {
        my $pos := nqp::list_i();
        my $tsize := nqp::chars($text);
        my $realbefore;
        my $maybefore := 0;
        my $line := 0;

        # count up the newlines

        while nqp::isle_i($maybefore, $at) {
            $line := nqp::add_i($line, 1);
            $realbefore := $maybefore;
            $maybefore := nqp::findcclass(nqp::const::CCLASS_NEWLINE, $text, $maybefore + 1, $tsize);
        }

        my $col := nqp::sub_i($at, $realbefore);

        nqp::push_i($pos, $line);
        nqp::push_i($pos, $col);
        $pos;
    }

    # XXX Use ANSIColor
    # simple parse failure
    method parse-fail($msg = "No message was given") {
        $/ = self.MATCH();

        my $string := shim-unbox_s($/.orig);

        my $failat := self.linecol($string, shim-unbox_i($/.from));

        my $fline := nqp::atpos_i($failat, 0);
        my $fcol  := nqp::sub_i(nqp::atpos_i($failat, 1), 1);

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
        
}

grammar Pod6::Grammar does GramError {
    token TOP {
        :my @*VM_MARGINS := nqp::list();
        <.blank_line>*
        [<block>
         <.blank_line>*]+
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
            nqp::push(@*VM_MARGINS, nqp::chars($margin));
        } else {
            nqp::push(
                @*VM_MARGINS,
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
        nqp::pop(@*VM_MARGINS);
        self;
    }

    token start_line {
        ^^    " " ** {@*VM_MARGINS[*-1]}
    }

    token end_line {
        $$ [\n || $ || <.parse-fail("Unknown line ending found.")>]
    }

    token blank_line {
        ^^ \h* <.end_line>
    }

    # high-level block handling

    token block {
        ^^ $<litmargin>=(\h*) <?before \= <-[=]> >
        {self.prime_line(~$<litmargin>)}

        <block_kind>

        <.unprime_line>
    }

    proto token block_kind {*}

    multi token block_kind:sym<delim> {
        "=begin" <.ws> # XXX want :: here
            <block_name> <.ws>
            <configopt> *%% <.ws> <.end_line>

        <extra_config_line>*

        # ...

        <.start_line> "=end" <.ws> [$<block_name>
                                   || <badname=block_name> {$<badname>.CURSOR.parse-fail("End block doesn't match start block")}
                                   ] <.ws> <.end_line>
    }

    multi token block_kind:sym<para> {
        "=for" <.ws> # XXX want :: here
            <block_name> <.ws>
            <configopt> *%% <.ws> <.end_line>

        <extra_config_line>*

        #...

        <.blank_line>
    }

    multi token block_kind:sym<abbr> {
        \= <block_name> <.ws>

        #...

        <.blank_line>
    }

    token block_name {
        || [<standard_name> | <semantic_standard_name>]
        || <not_name> { $/.CURSOR.parse-fail("Cannot use \"$<not_name>\" as block name") }
        || <reserved_name> { $/.CURSOR.parse-fail("Name \"$<reserved_name>\" is reserved for possible future definition") }
        || $<typename>=(<.alpha> <.alnum>+)
    }

    token standard_name {
        | code
        | comment
        | data
        | defn
        | finish
        | head
        | input
        | item
        | nested
        | output
        | para
        | pod
        | table
    }

    token semantic_standard_name {
        | NAME           | NAMES
        | AUTHOR         | AUTHORS
        | VERSION        | VERSIONS
        | CREATED
        | EMULATES
        | EXCLUDES
        | SYNOPSIS       | SYNOPSES
        | DESCRIPTION    | DESCRIPTIONS
        | USAGE          | USAGES
        | INTERFACE      | INTERFACES
        | METHOD         | METHODS
        | SUBROUTINE     | SUBROUTINES
        | OPTION         | OPTIONS
        | DIAGNOSTIC     | DIAGNOSTICS
        | ERROR          | ERRORS
        | WARNING        | WARNINGS
        | DEPENDENCY     | DEPENDENCIES
        | BUG            | BUGS
        | SEE\-ALSO
        | ACKNOWLEDGMENT | ACKNOWLEDGEMENTS
        | COPYRIGHT      | COPYRIGHTS
        | DISCLAIMER     | DISCLAIMERS
        | LICENCE        | LICENCES
        | LICENSE        | LICENSES
        | TITLE          | TITLES
        | SECTION        | SECTIONS
        | CHAPTER        | CHAPTERS
        | APPENDIX       | APPENDICES
        | TOC
        | INDEX          | INDICES
        | FOREWORD       | FOREWORDS
        | SUMMARY        | SUMMARIES
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

    # unfortunately, even after transferring to CORE, we'll need our own rule to
    # handle config options, because we can only accept constant keys and
    # values, but Perl6::Grammar's parser allow non-constants
    token configopt {
        :my $*ADV_BINARY := -1;
        [
        | \: [\! {$*ADV_BINARY := 0}]?
          $<ckey>=[<.ident> +% \-]
          $<cvalue>=[ <?{$*ADV_BINARY != 0}>
              [
              | <podassociative>
              | <podpositional>
              | \< ~ \> $<podqw>=[ [[<!before \h | <?[>]>> .]+] +%% <.ws> ]
              | \( ~ \) [[<podstr>|<podint>]||<.non-const-term>]
              ] || {if $*ADV_BINARY != 0 { $*ADV_BINARY := 1 } }
                   [ <!before \s> <.parse-fail("Cannot negate adverb with given value")> ]?
          ]
        | $<ckey>=[<.ident> +% \-]
          <.ws> "=>" <.ws>
          $<cvalue>=[
              [
              | <podint>
              | <podstr>
              | <podpositional>
              | <podassociative>
              ] || <.non-const-term>
                || {$/.CURSOR.parse-fail("Nothing found for fatarrow key; please use :$<ckey> if you meant to set a binary flag")}
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
        | <?before <+[$@%&]> | "::"> (\S\S?! <.ident>) {$/.CURSOR.parse-fail("Variable \"$0\" found in pod configuration; only constants are allowed")}
        | "#" <.parse-fail("Unexpected # in pod configuration. (Were you trying to comment out something?)")>
        | (<.alnum>+) {$/.CURSOR.parse-fail("Unknown term \"$0\" in configuration. Only constants are allowed.")}
    }

    token extra_config_line {
        <.start_line> \= \h+
        <configopt> +%% <.ws>
        <.end_line>
    }
}

# TEMP TEST

say Pod6::Grammar.parse(q:to/NOTPOD/);
    =begin pod :!autotoc
    =       :autotoc :imconfused :!ok
    =end Sod
NOTPOD