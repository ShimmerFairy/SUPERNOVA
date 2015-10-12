# World.pm6 --- new stuff for the World

use v6;
use nqp;

# TOCORE these methods should be added to Perl6::World (except the fake world
# methods). They're put there because they don't really fit as either a grammar
# or action method

class Earth {
    #### 'Fake' world methods (Those that already exist in World, but need to be
    #### faked here. Trying to use the real World is just a pain.)
    method add_constant($typename, 'type_new', **@args, *%conf) {
        my \a = ::($typename).new(|@args, |%conf);
        a;
    }

    method find_symbol(@typename) { ::(@typename.join('::')) }

    method add_object($a) { $a }

    #### 'New' world methods (TOCORE stuff to put in Perl6::World)

    # namely for "canonicalizing" the semantic block names, de-pluralizing and
    # in a couple cases americanizing the spelling (hey, I speak American
    # English, Larry does too, seems like the 'right' dialect to go with)
    method pod_normalize_block_name($name) {
        if nqp::isne_s($name, nqp::uc($name)) { # not a semantic name, this is easy
            return $name;
        }

        my $newname := $name;

        # first off, remove the plural
        # XXX TOCORE should be %hash
        my $hash := nqp::hash(
            "NAMES", "NAME",                     "AUTHORS", "AUTHOR",
            "VERSIONS", "VERSION",               "CREATEDS", "CREATED",
            "EMULATESES", "EMULATES",            "EXCLUDESES", "EXCLUDES",
            "SYNOPSES", "SYNOPSIS",              "DESCRIPTIONS", "DESCRIPTION",
            "USAGES", "USAGE",                   "INTERFACES", "INTERFACE",
            "METHODS", "METHOD",                 "SUBROUTINES", "SUBROUTINE",
            "OPTIONS", "OPTION",                 "DIAGNOSTICS", "DIAGNOSTIC",
            "ERRORS", "ERROR",                   "WARNINGS", "WARNING",
            "DEPENDENCIES", "DEPENDENCY",        "BUGS", "BUG",
            "SEE-ALSOS", "SEE-ALSO",             "ACKNOWLEDGEMENTS", "ACKNOWLEDGEMENT",
            "ACKNOWLEDGMENTS", "ACKNOWLEDGMENT", "COPYRIGHTS", "COPYRIGHT",
            "DISCLAIMERS", "DISCLAIMER",         "LICENCES", "LICENCE",
            "LICENSES", "LICENSE",               "TITLES", "TITLE",
            "SECTIONS", "SECTION",               "CHAPTERS", "CHAPTER",
            "APPENDICES", "APPENDIX",            "TOCS", "TOC",
            "INDICES", "INDEX",                  "INDEXES", "INDEX",
            "FOREWORDS", "FOREWORD",             "SUMMARIES", "SUMMARY"
        );

        if nqp::existskey($hash, $newname) {
            $newname := nqp::atkey($hash, $newname);
        }

        # now to normalize regional spelling
        $hash := nqp::hash("ACKNOWLEDGEMENT", "ACKNOWLEDGMENT", "LICENCE", "LICENSE");

        if nqp::existskey($hash, $newname) {
            $newname := nqp::atkey($hash, $newname);
        }

        $newname;
    }

    # gets the configuration for a particular block
    method pod_config_for_block($name) {
        my $normd := self.pod_normalize_block_name($name);
        # XXX TOCORE de-Perl6-ize

        if @*CONFIG_INFO[*-1]{$normd}:exists {
            @*CONFIG_INFO[*-1]{$normd}
        } elsif nqp::substr($normd, 0, 4) eq 'head' { # get general head config
            @*CONFIG_INFO[*-1]<head*>;
        } elsif nqp::substr($normd, 0, 4) eq 'item' { # get general item config
            @*CONFIG_INFO[*-1]<item*>;
        } else { # get default config
            @*CONFIG_INFO[*-1]<=default>;
        }
    }

    # gets the configuration for a particular formatting code
    method pod_config_for_format_code($name) {
        @*CONFIG_INFO[*-1]{$name ~ '<>'};
    }

    # checks if you need to preserve spaces in pod text
    method pod_preserve_spaces { ?%*THIS_CONFIG<keep-space> }

    # checks if the contents are to be verbatim (that is, no formatting code
    # parsing)
    method pod_verbatim_text { ?%*THIS_CONFIG<verbatim> }

    # checks if the given block implies paragraph and code blocks
    method pod_imply_blocks { ?%*THIS_CONFIG<implies-blocks> }

    # checks if the given formatting code is allowed to be used (allowed if
    # either not verbatim, or in the :allow list)
    method pod_allow_format_code($code) {
        # the odd do {} block is because NQP doesn't have any array-search ops
        !self.pod_verbatim_text || (
            %*THIS_CONFIG<allow> ??
            do { my $a := 0; $a := 1 if $_ eq $code for %*THIS_CONFIG<allow>; $a } !!
            False
        )
    }
}