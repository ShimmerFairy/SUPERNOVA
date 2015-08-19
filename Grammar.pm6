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

use Grammar::Tracer;

grammar Pod6::Grammar {
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
#        my @de-tabbed := nqp::split("\t", shim-unbox_s($margin));
#
#        if nqp::elems($de-tabbed) == 1 {
#            nqp::push(@*VM_MARGINS, nqp::chars($margin));
#        } else {
#            nqp::push(
#                @*VM_MARGINS,
#                nqp::chars(
#                    nqp::join(
#                        nqp::x(
#                            " ",
#                            ($?TABSTOP // 8)),
#                        $de-tabbed)));
#        }

        my @de-tabbed = $margin.split("\t");
        if +@de-tabbed == 1 {
            nqp::push(@*VM_MARGINS, $margin.chars);
        } else {
            nqp::push(@*VM_MARGINS, @de-tabbed.join(" " x ($?TABSTOP // 8)).chars);
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
        $$ [\n || $ || {die "Line ended surprisngly! XXX BETTER"}]
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

        <.start_line> "=end" <.ws> [$<block_name>||<.block_name> {die "NOT SAME XXX BETTER"}] <.ws> <.end_line>
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
        || <not_name> { die "Cannot use \"$<not_name>\" as block name XXX BETTER" }
        || <reserved_name> { die "Name \"$<reserved_name>\" is reserved for possible future definition XXX BETTER" }
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

    token configopt { <!> }
    token extra_config_line { <!> }
}

# TEMP TEST

say Pod6::Grammar.parse(" =begin pod
 =end pod");