# DAST.pm6 --- The various DAST classes for Pod

use v6;

class DAST::Piece does Associative {
    has Match $.node;
    has Str %!config;

    multi method hash(DAST::Piece:D:)                          { %!config }
    multi method AT-KEY(DAST::Group:D: Str $key) is rw         { %!config.AT-KEY($key) }
    multi method EXISTS-KEY(DAST::Group:D: Str $key)           { %!config.EXISTS-KEY($key) }
    multi method DELETE-KEY(DAST::Group:D: Str $key)           { %!config.DELETE-KEY($key) }
    mutli method ASSIGN-KEY(DAST::Group:D: Str $key, Str $val) { %!config.ASSIGN-KEY($key, $val) }
    # don't support BIND-KEY

    method node-gist(DAST::Piece:D:) {
        with $!node {
            " {$!node.Str}"
        } else {
            ""
        }
    }

    multi method gist(DAST::Piece:D:) {
        "Generic DAST piece" ~ node-gist
    }
}

class DAST::Group is DAST::Piece does Positional is Iterable {
    has DAST::Piece @!children;

    multi method list(DAST::Group:D:)                  { @!children }
    multi method elems(DAST::Group:D:)                 { @!children.elems }
    multi method AT-POS(DAST::Group:D: $idx) is rw     { @!children.AT-POS($idx) }
    multi method EXISTS-POS(DAST::Group:D: $idx)       { @!children.EXISTS-POS($idx) }
    multi method DELETE-POS(DAST::Group:D: $idx)       { @!children.DELETE-POS($idx) }
    multi method ASSIGN-POS(DAST::Group:D: $idx, $val) { @!children.ASSIGN-POS($idx, $val) }
    # don't support BIND-POS

    multi method gist(DAST::Group:D: Str $group-name) {
        my $gist = $group-name ~ node-gist ~ "\n";
        for @!children {
            $gist ~= $_.gist.indent(2) ~ "\n";
        }
        $gist.chomp;
    }

    multi method gist(DAST::Group:D:) {
        nextwith("DAST Group");
    }
}

class DAST::RawText is DAST::Piece {
    has Str $.text;

    method new($text) {
        self.bless(:$text);
    }

    multi method gist(DAST::RawText:D:) {
        "DAST raw text" ~ node-gist ~ "\n" ~ $!text.perl.ident(2)
    }
}

role DAST::FormatCode[Str $letter] is DAST::Group {
    multi method gist(DAST::FormatCode:D:) {
        nextwith("DAST {$letter}<>");
    }
}

class DAST::FormatCode::A does DAST::FormatCode["A"] { }
class DAST::FormatCode::B does DAST::FormatCode["B"] { }
class DAST::FormatCode::C does DAST::FormatCode["C"] { }
# D is special
class DAST::FormatCode::E does DAST::FormatCode["E"] { }
# F non-existent
# G non-existent
# H non-existent
class DAST::FormatCode::I does DAST::FormatCode["I"] { }
# J non-existent
class DAST::FormatCode::K does DAST::FormatCode["K"] { }
# L is special
# M is special
class DAST::FormatCode::N does DAST::FormatCode["N"] { }
# O non-existent
# P is special
# Q non-existent
class DAST::FormatCode::R does DAST::FormatCode["R"] { }
class DAST::FormatCode::S does DAST::FormatCode["S"] { }
class DAST::FormatCode::T does DAST::FormatCode["T"] { }
class DAST::FormatCode::U does DAST::FormatCode["U"] { }
class DAST::FormatCode::V does DAST::FormatCode["V"] { }
# W non-existent
# X is speical
# Y non-existent
class DAST::FormatCode::Z does DAST::FormatCode["Z"] { }

class DAST::FormatCode::D is DAST::Piece {
    has DAST::Group $.defined-term; # XXX  @.defined-term is DAST::Group  NYI
    has Str @.synonyms;

    multi method gist(DAST::FormatCode::D:D:) {
        my $gist = "DAST D<>" ~ node-gist ~ "\n";
        $gist ~= "  Defined Term:\n";
        for $!defined-term {
            $gist ~= $_.gist.indent(4) ~ "\n";
        }

        if +@!synonyms {
            $gist ~= "  Synonyms:\n";
            for @!synonyms {
                $gist ~= $_.gist.indent(4) ~ "\n";
            }
        }

        $gist.chomp
    }
}

# E

class DAST::FormatCode::L is DAST::Piece {
    has DAST::Group $.display-text;
    has Str $.scheme;
    has Str $.link;

    multi method gist(DAST::FormatCode::L:D:) {
        my $gist = "DAST L<>" ~ node-gist ~ "\n";
        $gist ~= "  Link to: $!scheme:$!link\n";
        if +$!display-text {
            $gist ~= "  Displays as:\n";
            for $!display-text {
                $gist ~= $_.gist.indent(4) ~ "\n";
            }
        }
        $gist.chomp
    }
}

class DAST::FormatCode::M is DAST::Piece {
    has Str $.verbatim-text;

    multi method gist(DAST::FormatCode::M:D:) {
        "DAST M<> {self.WHAT}" ~ node-gist; # .WHAT gives us () already
    }

    method M-type() { !!! }
}

class DAST::FormatCode::P is DAST::Piece {
    has Str $.scheme;
    has Str $.link;

    multi method gist(DAST::FormatCode::P:D:) {
        "DAST P<>" ~ node-gist ~ "\n" ~ "  Insert: $!scheme:$!link"
    }
}

class DAST::FormatCode::X is DAST::Piece {
    has DAST::Group $.display-text;
    has %.entries; # format entry => [] or entry => [subentry, subentry]

    method add-entry(Str $entry) {
        unless %!entries{$entry}:exists {
            %!entries{$entry} = Array.new;
        }
    }

    method add-subentry(Str $entry, Str $subent) {
        add-entry($entry); # to ensure we get the right list type in there
        %!entries{$entry}.push($entry);
    }

    method get-entries { # for getting just the entries, without needing the subentries
        %!entries.map: { $_.key };
    }

    multi method gist(DAST::FormatCode::X:D:) {
        my $gist = "DAST X<>" ~ node-gist ~ "\n";

        $gist ~= "  Displays As: " ~ $!display-text.gist.indent(4).trim ~ "\n";

        for %!entries {
            $gist ~= "  Entry {$_.key.perl}\n";
            for $_.value {
                $gist ~= "    Subentry {$_.perl}\n";
            }
        }

        $gist.chomp
    }
}

class DAST::Block::code    is DAST::Group { multi method gist(DAST::Block::code:D:) { nextwith("DAST Block code") } }
class DAST::Block::comment is DAST::Group { multi method gist(DAST::Block::code:D:) { nextwith("DAST Block comment") } }
class DAST::Block::defn    is DAST::Group { multi method gist(DAST::Block::code:D:) { nextwith("DAST Block defn") } }
class DAST::Block::input   is DAST::Group { multi method gist(DAST::Block::code:D:) { nextwith("DAST Block input") } }
class DAST::Block::nested  is DAST::Group { multi method gist(DAST::Block::code:D:) { nextwith("DAST Block nested") } }
class DAST::Block::output  is DAST::Group { multi method gist(DAST::Block::code:D:) { nextwith("DAST Block output") } }
class DAST::Block::para    is DAST::Group { multi method gist(DAST::Block::code:D:) { nextwith("DAST Block para") } }
class DAST::Block::pod     is DAST::Group { multi method gist(DAST::Block::code:D:) { nextwith("DAST Block pod") } }
class DAST::Block::data    is DAST::Group { multi method gist(DAST::Block::code:D:) { nextwith("DAST Block data") } }
class DAST::Block::finish  is DAST::Group { multi method gist(DAST::Block::code:D:) { nextwith("DAST Block finish") } }

class DAST::Block::head is DAST::Group {
    my Int $.level;
    multi method gist(DAST::Block::head:D:) {
        nextwith("DAST Block head, level $!level");
    }
}

class DAST::Block::item is DAST::Group {
    my $.level;
    multi method gist(DAST::Block::item:D:) {
        nextwith("DAST Block item, level $!level");
    }
}

class DAST::Block::table::Cell is DAST::Group {
    my $.row;
    my $.col;
    my $.rowspan;
    my $.colspan;
    my Bool $.header;

    multi method gist(DAST::Block::table::Cell:D:) {
        my $name = $!header ?? "Header cell " !! "Cell ";
        $name ~= "($!row, $!col), size $!rowspan x $!colspan";
        nextwith($name);
    }
}

class DAST::Block::table is DAST::Group {
    multi method gist(DAST::Block::table:D:) {
        nextwith("DAST Block table");
    }
}

class DAST::Block::Semantic is DAST::Group {
    has $.name;

    multi method gist(DAST::Block::Semantic:D:) {
        nextwith("DAST Semantic block $!name");
    }
}

class DAST::ExtBlock is DAST::Group {
    has $.block-name;

    multi method gist(DAST::Block::ExtBlock:D:) {
        nextwith("DAST Custom Block Type $.block-name");
    }
}