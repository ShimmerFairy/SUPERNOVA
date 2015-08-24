# Exception.pm6 --- Various Exceptions used by the grammar

use v6;

# TO-CORE this helper class will most likely have to be integrated into the
# classes that use them directly
class X::FLC {
    has $.file;
    has $.line;
    has $.col;

    method getloc {
        "$!file:$!line,$!col"
    }
}


# TO-CORE use the real group type instead
class X::Epitaph is Exception {
    has $.panic;
    has @.sorrows;
    has @.worries;

    method gist(X::Epitaph:D:) {
        my ($redbg, $reset) = !$*DISTRO.is-win ?? ("\e[41;1m", "\e[0m") !! ("", "");

        my $gist;
        $gist = "$redbg===SORRY!===$reset\n" if +@!sorrows || $!panic.defined;

        with $!panic {
            $gist ~= "Main issue:\n";
            $gist ~= $!panic.gist(:!singular).indent(4) ~ "\n";

            if +@!sorrows {
                $gist ~= "\nOther problems:\n";
            }
        } elsif +@!sorrows {
            $gist ~= "Problems:\n";
        }

        for @!sorrows {
            $gist ~= $_.gist(:!singular).indent(4) ~ "\n";
        }

        if +@!worries {
            if +@!sorrows || $!panic.defined {
                $gist ~= "\nOther potential difficulties:\n";
            } else {
                $gist ~= "Potential difficulties:\n";
            }
        }

        for @!worries {
            $gist ~= $_.gist(:!singular).indent(4) ~ "\n";
        }

        with $!panic {
            $gist ~= "\nThe main issue stopped parsing immediately. Please fix it so that we can parse more of the source code."
        } elsif +@!sorrows >= 10 { # XXX use real SORRY_LIMIT
            $gist ~= "\nThere were too many problems to continue parsing. Please fix some of them so that we can parse more of the source code."
        } elsif +@!sorrows {
            $gist ~= "\nThe problems above prevented the parser from producing something useful (however it was able to parse everything). Fixing them will allow useful output from the compiler.";
        } elsif +@!worries {
            $gist ~= "\nThe potential difficulties above may cause unexpected results, since they don't prevent the compiler from completing.\n";
            $gist ~= "Fix or suppress the issues as needed to avoid any doubt in execution.";
        } else {
            $gist ~= "\nSomehow threw an Epitaph without anything to actually throw. This likely indicates a deeper problem."
        }

        $gist
    }
}

# This should be doing X::Comp, but for simplicity sticking with Exception for
# now (so that the typical Exception-y stuff works)

class X::Pod6 is Exception {
    has $.v-margin;
    has $.goodpart;
    has $.badpart;

    has X::FLC $.err-flc;

    has $.hint-message;
    has $.hint-but-no-pointer;
    has $.hint-beforepoint;
    has $.hint-afterpoint;

    has X::FLC $.hint-flc;

    method gist(X::Pod6:D: :$singular = True) {
        my ($redbg, $red, $green, $yellow, $reset, $eject, $hintat) = !$*DISTRO.is-win
           ??
           ("\e[41;1m", "\e[31m", "\e[32m", "\e[33m", "\e[0m", "\c[EJECT SYMBOL]", "▶")
           !!
           ("", "", "", "", "", "<HERE>", "<THERE>");

        my $gist = $singular ?? "$redbg===SORRY!===$reset Issue in $!err-flc.file():\n" !! "";
        $gist ~= $.message ~ "\n";
        $gist ~= "at $!err-flc.getloc()\n";
        $gist ~= "------>|$green$.goodpart";
        $gist ~= "$yellow$eject";
        $gist ~= "{$red}{$.badpart.chomp}$reset";

        with $.hint-message {
            my $hint;
            $hint ~= "\n\n$.hint-message\n";
            unless $.hint-but-no-pointer {
                $hint ~= "at $!hint-flc.getloc()\n";
                $hint ~= "------>|$green$.hint-beforepoint";
                $hint ~= "$yellow$hintat";
                $hint ~= "{$green}{$.hint-afterpoint.chomp}$reset";
            }
            $gist ~= $hint.indent(4);
        }

        $gist;
    }
}

class X::Pod6::Didn'tComplete is X::Pod6 {
    method message() {
        "Pod parser ended before expected end point."
    }
}

class X::Pod6::MismatchedEnd is X::Pod6 {
    method message() {
        "Incorrect =end directive found for block";
    }

    method hint-message() {
        "Missing for =begin block here";
    }
}

class X::Pod6::BadConfig is X::Pod6 {
    has $.message;
    method message() {
        "Problem with config option: $!message";
    }
}

class X::Pod6::BadConfig::Comma is X::Pod6::BadConfig {
    method message() {
        "Commas not required for separating pod config options."
    }
}

class X::Pod6::Block::DirectiveAsName is X::Pod6 {
    has $.culprit;
    method message() {
        "Cannot use \"$.culprit\" as block name; please use =$.culprit if you meant to use the directive."
    }
}

class X::Pod6::Block::ReservedName is X::Pod6 {
    has $.culprit;
    method message() {
        "Cannot use \"$.culprit\" as block name, as it is reserved for future use " ~
        "(please vary the case if you want a custom block name, e.g. \"{$.culprit.tclc}\")"
    }
}

class X::Pod6::FCode::TooManyAngles is X::Pod6 {
    method message() {
        "Too many opening angle brackets in formatting code."
    }
}

class X::Pod6::FCode::ForcedStop is X::Pod6 {
    method message() {
        "Formatting code forced to stop before ending angle bracket."
    }
}

class X::Pod6::Encoding is X::Pod6 {
    has $.target-enc;
    method message() {
        "Changing encoding of Perl 6 files NYI. Please use Unicode text instead of \"$.target-enc.chomp().subst(/\s+/," ",:g)\" for now."
    }
}

class X::Pod6::Alias is X::Pod6 {
    has $.atype;

    method message() {
        "$.atype aliases NYI, sorry."
    }
}

class X::Pod6::ExtraEnd is X::Pod6 {
    method message() {
        "An unmatched =end directive was found"
    }

    method hint-message() {
        if $.hint-but-no-pointer {
            # XXX use X::Comp's "expecting" for those possibilites?
            "There was no delimited block before this. Perhaps you meant =begin (or =finish if expecting P5's __END__)?"
        } else {
            "The last =begin was here"
        }
    }
}