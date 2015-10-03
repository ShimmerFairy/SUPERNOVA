# GrammarError.pm6 --- role for handling errors in grammar parsing

# TO-CORE used only until we can move into core and use STD's stuff. Note that
# sorry/worry/panic are typed-sorry/worry/panic in core, with different arg
# styles

use v6;

use nqp;

sub shim-unbox_i(Int $a) { nqp::unbox_i($a) }
sub shim-unbox_s(Str $a) { nqp::unbox_s($a) }

sub shim-box_i(int $a) { nqp::box_i($a, Int) }

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
        my $linecol := self.linecol(shim-unbox_s($/.orig), shim-unbox_i($/.CURSOR.pos));

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
            %opts<hint-but-no-pointer> := 0;
        } else {
            %opts<hint-but-no-pointer> := 1;
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
        } elsif @*SORROWS > 0 {
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

        if +@*SORROWS || $panic.defined {
            $ghost.throw;
        } else {
            print($ghost.gist)
        }
    }
}