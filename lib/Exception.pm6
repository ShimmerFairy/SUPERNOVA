# Exception.pm6 --- Various Exceptions used by the grammar

use v6;

use Grammar::Parsefail::Exceptions;

# This should be doing X::Comp, but for simplicity sticking with Exception for
# now (so that the typical Exception-y stuff works)

class X::Pod6 is X::Grammar { }

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