# Actions.pm6 --- constructs a Pod6 tree of the given Pod

use v6;

use Pod6;

use nqp;

# TOCORE Just a fake world, since trying to interface with the real one is too
# much of a pain.
class FakeWorld {
    method add_constant($typename, 'type_new', **@args, *%conf) {
        my \a = ::($typename).new(|@args, |%conf);
        a;
    }
}

# TOCORE every |nqp::hllize(...) should be just |...

my $M = FakeWorld;

class Pod6::Actions {
    method start_document($/) {
        nqp::push(@*POD_BLOCKS, $M.add_constant('Pod6::Document', 'type_new'));
        @*POD_BLOCKS[0].push($M.add_constant('Pod6::Config', 'type_new'));
    }

    method new_block($/) {
        if nqp::istype($*BLOCK_NAME, Str) {
            # this branch of the conditional is for the 'odd' directives, ones
            # that aren't normal blocks.

            if $*BLOCK_NAME eq 'config' {
                nqp::push(@*POD_BLOCKS, $M.add_constant('Pod6::Config', 'type_new'));

                my $prevconfig = @*POD_BLOCKS[*-2].list.grep(* ~~ Pod6::Config)[*-1];

                @*POD_BLOCKS[*-1].inherit-config($prevconfig);

                # for extra config lines
                @*POD_BLOCKS[*-1].set-vmargin($*VMARGIN);
            } elsif $*BLOCK_NAME eq 'encoding' {
                nqp::push(@*POD_BLOCKS, $M.add_constant('Pod6::Encoding', 'type_new'));

                @*POD_BLOCKS[*-1].set-vmargin($*VMARGIN);
            } elsif $*BLOCK_NAME eq 'alias' {
                nqp::push(@*POD_BLOCKS, $M.add_constant('Pod6::Alias', 'type_new'));
            } else {
                die "Unknown!";
            }
        } else { # a delimited/paragraph/abbreviated block
            my $bname = $*BLOCK_NAME.ast;
            my $sname = ~$_ with $*BLOCK_NAME<semantic_standard_name>;
            my $level = +~$_ with $*BLOCK_NAME<standard_name><level>;

            nqp::push(@*POD_BLOCKS,
                      $M.add_constant($bname, 'type_new', :name($sname), :$level));

            # set the vmargin for this block
            @*POD_BLOCKS[*-1].set-vmargin($*VMARGIN);

            # next, we find the latest =config block, and clone it as the first
            # child of this new block (we only search within the eventual parent
            # block, because it's guaranteed to have one)

            my $confname = $sname ?? $sname !!
                           $*BLOCK_NAME<standard_name> ?? ~$*BLOCK_NAME<standard_name> !!
                           ~$*BLOCK_NAME<typename>;

            my $config = @*POD_BLOCKS[*-2].list.grep(* ~~ Pod6::Config)[*-1];

            @*POD_BLOCKS[*-1].push($config);

            # finally, apply any relevant configuration options to this block.

            my $opts := $config.grab-config-for($confname);

            @*POD_BLOCKS[*-1].set-config($opts);
        }
    }

#    method parent_block($/) {
#        my $child = nqp::pop(@*POD_BLOCKS);
#        @*POD_BLOCKS[*-1].push($child);
#    }

    method TOP($/) {
        @*POD_BLOCKS[*-1].push(nqp::hllize($_.ast)) for $<block>;
        die +@*POD_BLOCKS unless +@*POD_BLOCKS == 1;
        make @*POD_BLOCKS[0];
    }

    method block($/) {
        make nqp::pop(@*POD_BLOCKS);
    }

    method directive:sym<delim>($/) {
        my $blockitems := nqp::list();

        for $<contents> {
            next unless $_<block> || $_<pseudopara>;
            nqp::push($blockitems, $_<block> ?? $_<block>.ast !! $_<pseudopara>.ast);
        }
 
        @*POD_BLOCKS[*-1].push(nqp::hllize($blockitems));
    }

    method directive:sym<para>($/) { @*POD_BLOCKS[*-1].push($<pseudopara>.ast); }

    method directive:sym<abbr>($/) { @*POD_BLOCKS[*-1].push($<pseudopara>.ast); }

    method directive:sym<encoding>($/) { }#@*POD_BLOCKS[*-1].push(Pod6::Text::Plain.new("N ENC")) }
    method directive:sym<alias>($/) { }#@*POD_BLOCKS[*-1].push(Pod6::Text::Plain.new("N ALI")) }

    method directive:sym<config>($/)   {
        my $options := $<configset>.ast;

# XXX TOCORE 'make' hllizes stuff in P6, so this code doesn't work in P6.
#        for $<extra_config_line> {
#            for $_.ast {
#                nqp::bindkey($options, nqp::iterkey_s($_), nqp::iterval($_));
#            }
#        }

#        # TOCORE don't hllize
#        $options = nqp::hllize($options);

        for $<extra_config_line> {
            $options{$_.key} = $_.value for $_.ast;
        }

        my $setfor = ~$<thing>;

        for $options {
            @*POD_BLOCKS[*-1].add-config($setfor, $_.key, $_.value);
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

        for $<line> {
            $_<one_token_text>.map: { nqp::push($lines, $^a.ast) }

            # stick the used newline at the end of the line, for space
            # preservation
            nqp::push($lines, Pod6::Text::Plain.new(~$_<end_line>));
        }

        $lines
    }

    # XXX implement space de-preservation
    sub depreserve-text($lines) { $lines }

    method pseudopara:sym<implicit_code>($/) {
        # TOCORE should be @lines
        my $lines := collect-lines($/);

        make $M.add_constant('Pod6::Block::Code', 'type_new', |nqp::hllize($lines));
    }

    method pseudopara:sym<implicit_para>($/) {
        my $parts := depreserve-text(collect-lines($/));

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

    method block_config($/) {
        my $options := nqp::hash();

# TOCORE probably needs changing, P6 hllizes asts
        for $<configset>.ast {
            nqp::bindkey($options, $_.key, $_.value);
        }

        for $<extra_config_line> {
            for $_.ast {
                nqp::bindkey($options, $_.key, $_.value);
            }
        }

        @*POD_BLOCKS[*-1].add-to-config(nqp::hllize($options));
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
            $<value>.CURSOR.sorry(X::Pod6::BadConfig,
                                  message => 'Cannot negate colonpair and provide value at the same time') if $<neg>;

            make (~$<key> => ($<podint> ?? $<podint>.ast !!
                              $<podstr> ?? $<podstr>.ast !!
                              $<cgroup> ?? $<cgroup>.ast !!
                              $/.CURSOR.panic(X::Pod6::BadConfig, message => "Bad value: {$<value>.Str.perl}")));
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
                          $/.CURSOR.panic(X::Pod6::BadConfig, message => "Bad value: {$<value>.Str.perl}")));
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
}