# Actions.pm6 --- constructs a Pod6 tree of the given Pod

use v6;

use Pod6;

use nqp;

# TOCORE every |nqp::hllize(...) should be just |...

class Pod6::Actions {
    # head* and item* mean "any head/item". Starred configs are a fallback for
    # un-config'd specifics
    my %DEFAULTS := {
        # default defaults (for custom blocks) -- the = ensures it can't be a valid
        # block name
        "=default" => { :!verbatim, :!keep-space, :implies-blocks },

        # we explicitly give all the relevant values, even if !default would cover
        # them -- saves having to decide to lookup in !default
        "code"    => {  :verbatim,  :keep-space, :!implies-blocks, :nested },
        "comment" => { :!verbatim, :!keep-space, :!implies-blocks },
        "defn"    => { :!verbatim, :!keep-space,  :implies-blocks }, # :implies-blocks irrelevant for term line
        "head*"   => { :!verbatim, :!keep-space, :!implies-blocks },
        "input"   => { :!verbatim,  :keep-space, :!implies-blocks, :nested },
        "item*"   => { :!verbatim, :!keep-space,  :implies-blocks, :nested },
        "nested"  => { :!verbatim, :!keep-space,  :implies-blocks },
        "output"  => { :!verbatim,  :keep-space, :!implies-blocks, :nested },
        "para"    => { :!verbatim, :!keep-space, :!implies-blocks },
        "pod"     => { :!verbatim, :!keep-space,  :implies-blocks },
        "table"   => { :!verbatim, :!keep-space, :!implies-blocks }, # NOTE =table ignores these
        "data"    => { :!verbatim,  :keep-space, :!implies-blocks },
        "finish"  => { :!verbatim, :!keep-space,  :implies-blocks },

        "NAME"           => { :!verbatim, :!keep-space, :implies-blocks },
        "AUTHOR"         => { :!verbatim, :!keep-space, :implies-blocks },
        "VERSION"        => { :!verbatim, :!keep-space, :implies-blocks },
        "CREATED"        => { :!verbatim, :!keep-space, :implies-blocks },
        "EMULATES"       => { :!verbatim, :!keep-space, :implies-blocks },
        "EXCLUDES"       => { :!verbatim, :!keep-space, :implies-blocks },
        "SYNOPSIS"       => { :!verbatim, :!keep-space, :implies-blocks },
        "DESCRIPTION"    => { :!verbatim, :!keep-space, :implies-blocks },
        "USAGE"          => { :!verbatim, :!keep-space, :implies-blocks },
        "INTERFACE"      => { :!verbatim, :!keep-space, :implies-blocks },
        "METHOD"         => { :!verbatim, :!keep-space, :implies-blocks },
        "SUBROUTINE"     => { :!verbatim, :!keep-space, :implies-blocks },
        "OPTION"         => { :!verbatim, :!keep-space, :implies-blocks },
        "DIAGNOSTIC"     => { :!verbatim, :!keep-space, :implies-blocks },
        "ERROR"          => { :!verbatim, :!keep-space, :implies-blocks },
        "WARNING"        => { :!verbatim, :!keep-space, :implies-blocks },
        "DEPENDENCY"     => { :!verbatim, :!keep-space, :implies-blocks },
        "BUG"            => { :!verbatim, :!keep-space, :implies-blocks },
        "SEE-ALSO"       => { :!verbatim, :!keep-space, :implies-blocks },
        "ACKNOWLEDGMENT" => { :!verbatim, :!keep-space, :implies-blocks },
        "COPYRIGHT"      => { :!verbatim, :!keep-space, :implies-blocks },
        "DISCLAIMER"     => { :!verbatim, :!keep-space, :implies-blocks },
        "LICENSE"        => { :!verbatim, :!keep-space, :implies-blocks },
        "TITLE"          => { :!verbatim, :!keep-space, :implies-blocks },
        "SECTION"        => { :!verbatim, :!keep-space, :implies-blocks },
        "CHAPTER"        => { :!verbatim, :!keep-space, :implies-blocks },
        "APPENDIX"       => { :!verbatim, :!keep-space, :implies-blocks },
        "TOC"            => { :!verbatim, :!keep-space, :implies-blocks },
        "INDEX"          => { :!verbatim, :!keep-space, :implies-blocks },
        "FOREWORD"       => { :!verbatim, :!keep-space, :implies-blocks },
        "SUMMARY"        => { :!verbatim, :!keep-space, :implies-blocks },

        # formatting codes

        # A<> is the odd one out, since it's read more-or-less as a code snippet, so
        # the standard config options don't apply. Not defined here for that reason.

        "B<>" => { :!verbatim, :!keep-space },
        "C<>" => {  :verbatim,  :keep-space },
        "D<>" => { :!verbatim, :!keep-space },  # display text only
        # E<> ignores config opts, since it parses specially
        # F<> reserved
        # G<> reserved
        # H<> reserved
        "I<>" => { :!verbatim, :!keep-space },
        # J<> reserved
        "K<>" => { :!verbatim,  :keep-space },
        "L<>" => { :!verbatim, :!keep-space },  # display text only
        "M<>" => {  :verbatim,  :keep-space },  # for all M<> codes; specifics as M<scheme>
        "N<>" => { :!verbatim, :!keep-space },
        # O<> reserved
        "P<>" => { :!verbatim, :!keep-space },  # display text only (and only for L<> fallback)
        # Q<> reserved
        "R<>" => { :!verbatim, :!keep-space },
        "S<>" => { :!verbatim,  :keep-space },
        "T<>" => { :!verbatim,  :keep-space },
        "U<>" => { :!verbatim, :!keep-space },
        "V<>" => {  :verbatim, :!keep-space },
        # W<> reserved
        "X<>" => { :!verbatim, :!keep-space },  # display text only
        # Y<> reserved
        "Z<>" => { :!verbatim, :!keep-space },
    };
    method start_document($/) {
        nqp::push(@*CONFIG_INFO, %DEFAULTS);
    }

    method new_scope($/) {
        nqp::push(@*CONFIG_INFO, @*CONFIG_INFO[*-1]);

        %*THIS_CONFIG := $*W.pod_config_for_block($*BLOCK_NAME);
    }

    method finish_scope($/) {
        nqp::pop(@*CONFIG_INFO);
    }

    method TOP($/) {
        my $blocks := nqp::list();

        for $<block> {
            nqp::push($blocks, $_.ast);
        }

        make $*W.add_constant('Pod6::Document', 'type_new', |nqp::hllize($blocks));
    }

    method block($/) { make $<directive>.ast }

    method directive:sym<delim>($/) {
        my $parts := nqp::list();
        my $name := $<block_name><semantic_standard_name>
                    ?? $*W.pod_normalize_block_name(~$<block_name><semantic_standard_name>)
                    !! "";
        my $level := $<block_name><standard_name><level>
                     ?? +~$<block_name><standard_name><level>
                     !! 1;

        for $<contents> {
            if $_<block> {
                nqp::push($parts, $_<block>.ast);
            } elsif $_<pseudopara> {
                nqp::push($parts, $_<pseudopara>.ast);
            }
        }

        make $*W.add_constant($<block_name>.ast, 'type_new', $*TERM_LINE, |nqp::hllize($parts),
                              :$name, :$level, :term($*TERM_LINE), :config(%*THIS_CONFIG));
    }

    method directive:sym<para>($/) {
        my $name := $<block_name><semantic_standard_name>
                    ?? $*W.pod_normalize_block_name(~$<block_name><semantic_standard_name>)
                    !! "";
        my $level := $<block_name><standard_name><level>
                     ?? +~$<block_name><standard_name><level>
                     !! 1;

        if nqp::istype($<pseudopara>.ast, Pod6::Excerpt) {
            make $*W.add_constant($<block_name>.ast, 'type_new', $*TERM_LINE, $<pseudopara>.ast,
                                  :$name, :$level, :term($*TERM_LINE), :config(%*THIS_CONFIG));
        } else {
            make $*W.add_constant($<block_name>.ast, 'type_new', $*TERM_LINE, |$<pseudopara>.ast,
                                  :$name, :$level, :term($*TERM_LINE), :config(%*THIS_CONFIG));
        }
    }

    method directive:sym<abbr>($/) {
        my $name := $<block_name><semantic_standard_name>
                    ?? $*W.pod_normalize_block_name(~$<block_name><semantic_standard_name>)
                    !! "";
        my $level := $<block_name><standard_name><level>
                     ?? +~$<block_name><standard_name><level>
                     !! 1;

        if nqp::istype($<pseudopara>.ast, Pod6::Excerpt) {
            make $*W.add_constant($<block_name>.ast, 'type_new', $<pseudopara>.ast,
                                  :$name, :$level, :term($*TERM_LINE), :config(%*THIS_CONFIG));
        } else {
            make $*W.add_constant($<block_name>.ast, 'type_new', |$<pseudopara>.ast,
                                  :$name, :$level, :term($*TERM_LINE), :config(%*THIS_CONFIG));
        }
    }

    method directive:sym<encoding>($/) { }
    method directive:sym<alias>($/) { }

    method directive:sym<config>($/) {
        # TOCORE need to change for NQP; make hllizes the config asts.

        my $setfor = ~$<thing>;

        for $<configset>.ast {
            @*CONFIG_INFO[*-1]{$setfor}{$_.key} = $_.value;
        }

        for $<extra_config_line> {
            for $_.ast {
                @*CONFIG_INFO[*-1]{$setfor}{$_.key} = $_.value;
            }
        }
    }

    method block_name($/) {
        my $name;
        if $<standard_name> {
            $name = "Pod6::Block::{$<standard_name>.Str.tclc}";
        } elsif $<semantic_standard_name> {
            $name = "Pod6::Block::SEMANTIC";
        } elsif $<typename> {
            $name = "Pod6::MBlock::{~$<typename>}";
        } else {
            die "Unknown block_name!";
        }
        make $name;
    }

    sub collect-lines($/) {
        my $lines := nqp::list();
        my $lineno := 0;
        for $<line> {
            for $_<one_token_text> {
                if nqp::istype($_.ast, Pod6::Text::Plain) && nqp::elems($lines) &&
                   nqp::istype($lines[*-1], Pod6::Text::Plain) {
                    $lines[*-1].append($_.ast);
                } else {
                    nqp::push($lines, $_.ast);
                }
            }

            # stick the used newline at the end of the line, for space
            # preservation

            if nqp::istype($lines[*-1], Pod6::Text::Plain) {
                $lines[*-1].append(~$_<end_line>);
            } else {
                nqp::push($lines, $*W.add_constant('Pod6::Text::Plain', 'type_new', ~$_<end_line>));
            }

            $lineno := nqp::add_i($lineno, 1);

            # if we're in a =defn, then take the first line for the term (this
            # is the least painful place to do this, for those curious.)
            if $lineno == 1 && $*BLOCK_NAME eq 'defn' {
                $*TERM_LINE := nqp::list();
                while nqp::elems($lines) {
                    nqp::push($*TERM_LINE, nqp::shift($lines));
                }
            }
        }

        $lines
    }

    # normalizes all spans of spaces to one U+0020 character. Yes, even if the
    # span is just one character that's not a space character (so NBSP users
    # will need S<> or such to keep it, for example). I've decided to not worry
    # about maintaing correct space characters for now, since that seems to
    # count under not preserving spaces.
    sub depreserve-text($parts) {
        my $newparts := nqp::list();

        for $parts.list -> $PART {
            if nqp::istype($PART, Pod6::Text::FormatCode) {
                nqp::push($newparts, $PART);
                next;
            }

            my $newtext := nqp::unbox_s($PART.text);
            my $ws-start;
            my $ws-end;

            $ws-start := nqp::findcclass(nqp::const::CCLASS_WHITESPACE, $newtext,
                                         0, nqp::chars($newtext));

            while $ws-start < nqp::chars($newtext) {
                $ws-end := nqp::findnotcclass(nqp::const::CCLASS_WHITESPACE, $newtext,
                                              $ws-start, nqp::chars($newtext) - $ws-start);

                # do a trim if it's whitespace at the start or end of the
                # collection, otherwise replace with a single space
                if nqp::elems($newparts)     == 0            && $ws-start == 0 ||
                   nqp::elems($newparts) + 1 == $parts.elems && $ws-end   == nqp::chars($newtext) {
                    $newtext := nqp::replace($newtext, $ws-start, $ws-end - $ws-start, "");
                } else {
                    $newtext := nqp::replace($newtext, $ws-start, $ws-end - $ws-start, " ");
                }

                $ws-start := nqp::findcclass(nqp::const::CCLASS_WHITESPACE, $newtext,
                                             $ws-start + 1, nqp::chars($newtext) - $ws-start - 1);
            }

            nqp::push($newparts, $*W.add_constant('Pod6::Text::Plain', 'type_new', $newtext));
        }

        $newparts;
    }
                

    method pseudopara:sym<implicit_code>($/) {
        # TOCORE should be @lines
        my $lines := collect-lines($/);
        $lines := depreserve-text($lines) unless $*W.pod_preserve_spaces;

        if $*BLOCK_NAME eq 'defn' {
            $*TERM_LINE := depreserve-text($*TERM_LINE) unless $*W.pod_preserve_spaces;
            $*TERM_LINE := $*W.add_constant('Pod6::Block::Code', 'type_new', |nqp::hllize($*TERM_LINE), :config(%*THIS_CONFIG));
        }

        make $*W.add_constant('Pod6::Block::Code', 'type_new', |nqp::hllize($lines), :config(%*THIS_CONFIG));
    }

    method pseudopara:sym<implicit_para>($/) {
        my $parts := depreserve-text(collect-lines($/));
        $parts := depreserve-text($parts) unless $*W.pod_preserve_spaces;

        if $*BLOCK_NAME eq 'defn' {
            $*TERM_LINE := depreserve-text($*TERM_LINE) unless $*W.pod_preserve_spaces;
            $*TERM_LINE := $*W.add_constant('Pod6::Block::Para', 'type_new', |nqp::hllize($*TERM_LINE), :config(%*THIS_CONFIG));
        }

        make $*W.add_constant('Pod6::Block::Para', 'type_new', |nqp::hllize($parts), :config(%*THIS_CONFIG));
    }

    method pseudopara:sym<nothing_implied>($/) {
        my $lines := collect-lines($/);
        $lines := depreserve-text($lines) unless $*W.pod_preserve_spaces;

        if $*BLOCK_NAME eq 'defn' {
            $*TERM_LINE := depreserve-text($*TERM_LINE) unless $*W.pod_preserve_spaces;
            $*TERM_LINE := nqp::hllize($*TERM_LINE);
        }

        make nqp::hllize($lines);
    }

    method one_token_text($/) {
        if $<formatting_code> {
            make $<formatting_code>.ast;
        } else {
            make $*W.add_constant('Pod6::Text::Plain', 'type_new', ~$/);
        }
    }

    method block_config($/) {
        # TOCORE probably needs changing, P6 hllizes asts
        with $<configset> {
            for $<configset>.ast {
                %*THIS_CONFIG{$_.key} = $_.value;
            }
        }

        with $<extra_config_line> {
            for $<extra_config_line> {
                for $_.ast {
                    %*THIS_CONFIG{$_.key} = $_.value;
                }
            }
        }
    }

    method configset($/) {
        my $opts := nqp::hash();

        for $<config_option> {
            nqp::bindkey($opts, $_.ast.key, $_.ast.value);
        }

        make $opts;
    }

    method extra_config_line($/) {
        my $yetmore := nqp::hash();

# XXX TOCORE 'make' hllizes in P6, so $<configset>.ast is a P6 object
#        for nqp::iterator($<configset>.ast) {
#            nqp::bindkey($yetmore, nqp::iterkey_s($_), nqp::iterval($_));
#        }
        for $<configset>.ast {
            nqp::bindkey($yetmore, $_.key, $_.value);
        }

        make $yetmore;
    }

    method config_option:sym<colonpair>($/) {
        if $<value> {
            $<value>.CURSOR.typed_sorry(X::Pod6::BadConfig,
                                        message => 'Cannot negate colonpair and provide value at the same time') if $<neg>;

            make (~$<key> => ($<podint> ?? $<podint>.ast !!
                              $<podstr> ?? $<podstr>.ast !!
                              $<cgroup> ?? $<cgroup>.ast !!
                              $/.CURSOR.typed_panic(X::Pod6::BadConfig, message => "Bad value: {$<value>.Str.perl}")));
        } elsif $<neg> {
            make (~$<key> => 0);
        } else {
            make (~$<key> => 1);
        }
    }

    method config_option:sym<fatarrow>($/) {
        make (~$<key> => ($<podint> ?? $<podint>.ast !!
                          $<podstr> ?? $<podstr>.ast !!
                          $<cgroup> ?? $<cgroup>.ast !!
                          $/.CURSOR.typed_panic(X::Pod6::BadConfig, message => "Bad value: {$<value>.Str.perl}")));
    }

    method podint($/) { make +~$/ }
    method podstr($/) { make [~] $0.map: *.Str }

    method cgroup($/) {
        if $<sbitem> {
            my $items := nqp::list();
            nqp::push($items, $_<podint> ?? $_<podint>.ast !! $_<podstr>.ast) for $<sbitem>;

            make $items;
        } elsif $<qwitem> {
            my $items := nqp::list();
            nqp::push($items, ~$_) for $<qwitem>;

            make $items;
        } elsif $<cbitem> {
            my $opts := nqp::hash();

            for $<cbitem> {
                nqp::bindkey($opts, $_<config_option>.ast[0], $_<config_option>.ast[1]);
            }

            make $opts;
        }
    }

    method fcode_open($/) {
        %*THIS_CONFIG := $*W.pod_config_for_format_code($*FC);
    }

    method fcode_scheme($/) { make (~$/).tclc }

    method fcode_inside($/) {
        my $parts := nqp::list();

        for $/.caps {
            if $_<formatting_code> {
                nqp::push($parts, $_<formatting_code>.ast);
            } elsif $_<one> {
                if nqp::elems($parts) && nqp::istype($parts[*-1], Pod6::Text::Plain) {
                    $parts[*-1].append(~$_<one>);
                } else {
                    nqp::push($parts, $*W.add_constant('Pod6::Text::Plain', 'type_new', ~$_<one>));
                }
            } elsif $_<fcode_inside> {
                for $_<fcode_inside>.ast {
                    if nqp::istype($_, Pod6::Text::Plain) {
                        if nqp::elems($parts) && nqp::istype($parts[*-1], Pod6::Text::Plain) {
                            $parts[*-1].append($_);
                        } else {
                            nqp::push($parts, $_);
                        }
                    } else {
                        nqp::push($parts, $_);
                    }
                }
            } else {
                die "AH $_";
            }
        }

        make $parts
    }

    method formatting_code:sym<A>($/) {
        # supporting sigils will seem to require NQP features we just don't
        # have. ( ::('$?FILE') doesn't work, critically ) The =alias kind (for
        # non-ambient) should be available soon-ish.

        $/.CURSOR.panic("A<> NYI");
    }

    method formatting_code:sym<D>($/) {
        my $disptext := $<display> ?? $<display>.ast !! nqp::list(Pod6::Text::Plain.new(~$<syn>[0]));

        my $syns := nqp::list();

        nqp::push($syns, ~$_) for $<syn>;

        # knock off first definition if we had to take it for the display text
        # (and thus also main term)
        nqp::shift($syns) unless $<display>;

        $disptext := depreserve-text($disptext) unless $*W.pod_preserve_spaces;

        my $fc := $*W.find_symbol(['Pod6', 'Text', 'FormatCode', 'D']).new(|nqp::hllize($disptext), :config(%*THIS_CONFIG));

        $fc.set-term($fc.text);
        $fc.set-synonyms(nqp::hllize($syns));

        make $*W.add_object($fc); # XXX use a function that does the post-creation parts of add_constant?
    }

    method formatting_code:sym<E>($/) {
        my $convchars := "";

        for $<terms> {
            if $_<uname> {
                my $ord := nqp::codepointfromname(~$_<uname>);

                if nqp::iseq_i($ord, -1) {
                    $/.CURSOR.panic("Invalid Unicode name");
                }

                $convchars := nqp::concat($convchars, nqp::chr($ord));
            } else {
                my $ord;

                if $_<xnum> {
                    $ord := nqp::radix(16, ~$_<xnum>, 0, 0)[0];
                } elsif $_<onum> {
                    $ord := nqp::radix( 8, ~$_<onum>, 0, 0)[0];
                } elsif $_<bnum> {
                    $ord := nqp::radix( 2, ~$_<bnum>, 0, 0)[0];
                } elsif $_<dnum> {
                    $ord := nqp::radix(10, ~$_<dnum>, 0, 0)[0];
                }

                $convchars := nqp::concat($convchars, nqp::chr($ord));
            }
        }

        make $*W.add_constant('Pod6::Text::FormatCode::E', 'type_new', $convchars, :config(%*THIS_CONFIG));
    }

    method formatting_code:sym<LP>($/) {
        # use the scheme if it was found, or else default to doc (for L<>) or
        # die (for P<>).
        my $scheme := $<fcode_scheme> ?? $<fcode_scheme>.ast !!
                      ~$<fcode> eq "L" ?? "Doc" !! die "EGADS!";

        my $fc := $*W.find_symbol(['Pod6', 'Text', 'FormatCode', ~$<fcode>, $scheme.trim.tclc]).new(~$<address>, :config(%*THIS_CONFIG));
        my $PlainText := $*W.find_symbol(['Pod6', 'Text', 'Plain']);
        my $disptext := $<display> ?? $<display>.ast !! nqp::list($PlainText.new($fc.link));

        $disptext := depreserve-text($disptext) unless $*W.pod_preserve_spaces;

        $fc.push(|nqp::hllize($disptext));

        make $*W.add_object($fc); # XXX maybe not add_object?
    }

    method formatting_code:sym<M>($/) {
        my $dt := $<text>.ast;
        $dt := depreserve-text($dt) unless $*W.pod_preserve_spaces;

        make $*W.add_constant(nqp::concat('Pod6::Text::FormatCode::M::', $<fcode_scheme>.ast),
                             'type_new', |nqp::hllize($dt), :config(%*THIS_CONFIG));
    }

    method formatting_code:sym<X>($/) {
        my $PlainText := $*W.find_symbol(['Pod6', 'Text', 'Plain']);
        my $Pair := $*W.find_symbol(['Pair']);
        my $entries := nqp::list();

        my $disptext := $<display> ?? $<display>.ast !! nqp::list($PlainText.new(~$<entry>[0]<main>));
        $disptext := depreserve-text($disptext) unless $*W.pod_preserve_spaces;

        for $<entry> {
            my $main := depreserve-text(nqp::list($PlainText.new(~$_<main>)))[0].text;
            if $_<subent> {
                my $selist := nqp::list();
                my $pairing := 1;

                for $_<subent> {
                    # we want to process $selist 'in reverse', so use unshift
                    # instead of push
                    nqp::unshift($selist, ~$_);
                }

                # XXX TOCORE no hllize
                for nqp::hllize($selist) {
                    $pairing := $Pair.new($_, $pairing);
                }

                $pairing := $Pair.new($main, $pairing);

                nqp::push($entries, $pairing);
            } else {
                nqp::push($entries, $main);
            }
        }

        make $*W.add_constant('Pod6::Text::FormatCode::X', 'type_new', nqp::hllize($entries), |nqp::hllize($disptext), :config(%*THIS_CONFIG));
    }

    method formatting_code:sym<normal>($/) {
        my $dt := $<contents>.ast;
        $dt := depreserve-text($dt) unless $*W.pod_preserve_spaces;

        make $*W.add_constant(nqp::concat('Pod6::Text::FormatCode::', ~$<fcode>), 'type_new', |nqp::hllize($dt), :config(%*THIS_CONFIG));
    }
}