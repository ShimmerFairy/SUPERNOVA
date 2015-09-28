# Actions.pm6 --- constructs a Pod6 tree of the given Pod

use v6;

use Pod6;

use nqp;

# TOCORE Just a fake world, since trying to interface with the real one is too
# much of a pain.
class FakeWorld {
    method add_constant($typename, 'type_new', *@args, *%conf) {
        ::($typename).new(|@args, |%conf);
    }
}

# TOCORE every |nqp::hllize(...) should be just |...

my $M = FakeWorld;

class Pod6::Actions {
    method TOP($/) {
        make $<block>.map: {$_.ast};
    }

    method block($/) {
        say $<directive>.ast;
        make $<directive>.ast;
    }

    method directive:sym<delim>($/) {
        my $typename := $<block_name>.ast;
        my $blockitems := nqp::list();

        for $<contents> {
            next unless $_<block> || $_<pseudopara>;
            nqp::push($blockitems, $_<block> ?? $_<block>.ast !! $_<pseudopara>.ast);
        }

        if $<block_name><standard_name><level> -> $LVL {
            make $M.add_constant($typename, 'type_new', |nqp::hllize($blockitems), :level(+~$LVL));
        } else {
            make $M.add_constant($typename, 'type_new', |nqp::hllize($blockitems));
        }
    }

    method directive:sym<para>($/) {
        my $typename := $<block_name>.ast;

        if $<block_name><standard_name><level> -> $LVL {
            make $M.add_constant($typename, 'type_new', $<pseudopara>.ast, :level(+~$LVL));
        } else {
            make $M.add_constant($typename, 'type_new', $<pseudopara>.ast);
        }
    }

    method directive:sym<abbr>($/) {
        my $typename := $<block_name>.ast;

        if $<block_name><standard_name><level> -> $LVL {
            make $M.add_constant($typename, 'type_new', $<pseudopara>.ast, :level(+~$LVL));
        } else {
            make $M.add_constant($typename, 'type_new', $<pseudopara>.ast);
        }
    }

    method directive:sym<encoding>($/) { make Pod6::Text::Plain.new("N ENC") }
    method directive:sym<alias>($/)    { make Pod6::Text::Plain.new("N ALI") }
    method directive:sym<config>($/)   { make Pod6::Text::Plain.new("N CON") }

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

        for $<line> {
            $_<one_token_text>.map: { nqp::push($lines, $^a.ast) }

            # stick a newline at the end of the line, to preserve code formatting
            nqp::push($lines, Pod6::Text::Plain.new("\n"));
        }

        $lines
    }

    method pseudopara:sym<implicit_code>($/) {
        # TOCORE should be @lines
        my $lines := collect-lines($/);

        make $M.add_constant('Pod6::Block::Code', 'type_new', |nqp::hllize($lines));
    }

    method pseudopara:sym<implicit_para>($/) {
        my $parts := collect-lines($/);

        make $M.add_constant('Pod6::Block::Para', 'type_new', |nqp::hllize($parts));
    }

    method pseudopara:sym<nothing_implied>($/) {
        my $lines := collect-lines($/);

        make $lines;
    }

    method one_token_text($/) {
        if $<formatting_code> {
            make $<formatting_code>.ast;
        } else {
            make $M.add_constant('Pod6::Text::Plain', 'type_new', ~$/);
        }
    }

    method fc_content($/) {
        my $parts := nqp::list();

        for $<one_token_text> {
            nqp::push($parts, $_.ast);
        }

        make $parts;
    }

    method formatting_code:sym<D>($/) {
        my $defterm := $<defined>.ast;

        if $<synonym> {
            my $syns := nqp::list();

            for $<synonym> {
                nqp::push($syns, $_.ast);
            }

            make $M.add_constant('Pod6::Text::FCode::D', 'type_new', $defterm, |nqp::hllize($syns));
        } else {
            make $M.add_constant('Pod6::Text::FCode::D', 'type_new', $defterm);
        }
    }

    method formatting_code:sym<E>($/) {
        my $ents := nqp::list();

        for $<entity> {
            # TO-CORE find val() instead
            my $numeric := val(~$_);
            if nqp::istype($numeric, Int) {
                nqp::push($ents, nqp::chr($numeric.Int));
            } else {
                $numeric := nqp::codepointfromname(~$_);
                if $numeric > -1 {
                    nqp::push($ents, nqp::chr($numeric));
                } else {
                    # HTML5 entity non-support is intentional
                    die "OH NOT GOOD {~$_}";
                }
            }
        }

        make $M.add_constant('Pod6::Text::FCode::E', 'type_new', |nqp::hllize($ents));
    }

    method formatting_code:sym<L>($/) {
        my $url;
        my $displaytext;

        $displaytext := $<displayish>.ast;

        if $<definite_link> {
            $url := ~$<definite_link>;
        } else {
            $url := $displaytext.text;
        }

        make $M.add_constant('Pod6::Text::FCode::L', 'type_new', $displaytext, $url);
    }

    method formatting_code:sym<M>($/) {
        my $typename := "Pod6::Text::FCode::M::{~$<typename>}";
        my $contents := $<verbatim>.ast;

        make $M.add_constant($typename, 'type_new', $contents);
    }

    method formatting_code:sym<X>($/) {
        my $entries := nqp::hash();
        my $displaytext;

        $displaytext := $<displayish>.ast;

        if $<entry> {
            for $<entry> {
                my $ekey := ~$_<fc_content>;
                nqp::bindkey($entries, $ekey, nqp::list());
                for $<subentry> {
                    nqp::push(nqp::atkey($entries, $ekey), ~$_);
                }
            }
        } else {
            nqp::bindkey($entries, $displaytext.text, nqp::list());
        }

        make $M.add_constant('Pod6::Text::FCode::X', 'type_new', $displaytext, $entries);
    }

    method formatting_code:sym<normal>($/) {
        my $typename := "Pod6::Text::FCode::{~$<which>}";

        make $M.add_constant($typename, 'type_new', |$<fc_content>.ast);
    }
}