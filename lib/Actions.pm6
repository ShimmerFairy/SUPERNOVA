# Actions.pm6 --- constructs a Pod6 tree of the given Pod

use v6;

use Pod6;

use nqp;

# TOCORE Just a fake world, since trying to interface with the real one is too
# much of a pain.
class FakeWorld {
    method add_constant($typename, 'type_new', **@args, *%conf) {
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
        make $<directive>.ast;
    }

    method directive:sym<delim>($/) {
        my $typename := $<block_name>.ast;
        my $blockitems := nqp::list();

        my $name = "";
        $name = ~$_ with $<block_name><semantic_standard_name>;

        for $<contents> {
            next unless $_<block> || $_<pseudopara>;
            nqp::push($blockitems, $_<block> ?? $_<block>.ast !! $_<pseudopara>.ast);
        }

        if $<block_name><standard_name><level> -> $LVL {
            make $M.add_constant($typename, 'type_new', :$name, |nqp::hllize($blockitems), :level(+~$LVL));
        } else {
            make $M.add_constant($typename, 'type_new', :$name, |nqp::hllize($blockitems));
        }
    }

    method directive:sym<para>($/) {
        my $typename := $<block_name>.ast;

        my $name = "";
        $name = ~$_ with $<block_name><semantic_standard_name>;

        if $<block_name><standard_name><level> -> $LVL {
            make $M.add_constant($typename, 'type_new', :$name, $<pseudopara>.ast, :level(+~$LVL));
        } else {
            make $M.add_constant($typename, 'type_new', :$name, $<pseudopara>.ast);
        }
    }

    method directive:sym<abbr>($/) {
        my $typename := $<block_name>.ast;

        my $name = "";
        $name = ~$_ with $<block_name><semantic_standard_name>;

        if $<block_name><standard_name><level> -> $LVL {
            make $M.add_constant($typename, 'type_new', :$name, $<pseudopara>.ast, :level(+~$LVL));
        } else {
            make $M.add_constant($typename, 'type_new', :$name, $<pseudopara>.ast);
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
}