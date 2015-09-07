# Pod6.pm6 --- various Pod6 classes for holding stuff

#| The base class of Pod6 stuff. Supports just configuration stuff
class Pod6::Excerpt {
    has %.config;

    method new(*%config) {
        self.bless(:%config);
    }

    multi method hash(Pod6::Excerpt:D:)                          { %!config }
    multi method AT-KEY(Pod6::Excerpt:D: Str $key) is rw         { %!config.AT-KEY($key) }
    multi method EXISTS-KEY(Pod6::Excerpt:D: Str $key)           { %!config.EXISTS-KEY($key) }
    multi method DELETE-KEY(Pod6::Excerpt:D: Str $key)           { %!config.DELETE-KEY($key) }
    mutli method ASSIGN-KEY(Pod6::Excerpt:D: Str $key, Str $val) { %!config.ASSIGN-KEY($key, $val) }

    method give-config {
        @!children».take-config(%!config);
    }

    method take-config(%!inheritance) {
        for %!inheritance.kv -> $k, $v {
            unless %!config{$k}:exists {
                %!config{$k} = $v
            }
        }
    }

    multi method gist(Pod6::Excerpt:D:) {
        my $gist = "{self.^name}:\n";
        $gist ~= self.gist-children;
        $gist.chomp;
    }

    method gist-children { "" }
}

#| a role for Pod6 classes having children
role Pod6::Children[::ELEMT = Pod6::Excerpt] {
    has ELEMT @.children;

    multi method list(Pod6::Children:D:)                  { @!children }
    multi method elems(Pod6::Children:D:)                 { @!children.elems }
    multi method AT-POS(Pod6::Children:D: $idx) is rw     { @!children.AT-POS($idx) }
    multi method EXISTS-POS(Pod6::Children:D: $idx)       { @!children.EXISTS-POS($idx) }
    multi method DELETE-POS(Pod6::Children:D: $idx)       { @!children.DELETE-POS($idx) }
    multi method ASSIGN-POS(Pod6::Children:D: $idx, $val) { @!children.ASSIGN-POS($idx, $val) }

    method gist-children(Pod6::Children:D:) {
        my $childs = "";
        $childs ~= $_.gist for @!children;
        $childs.chomp;
    }
}

#| Contains text in a block. The "bottom" class in the tree
class Pod6::Text is Pod6::Excerpt does Pod6::Children[Str] {
    method new(*@children, *%config) {
        self.bless(:@children, :%config);
    }

    method text { [~] @!children }

    multi method gist(Pod6::Text:D:) {
        self.text.perl
    }
}

#| Contains a formatting code
class Pod6::FormatCode is Pod6::Excerpt does Pod6::Children {
    has Str $.name;

    method new(:$name, *@children, *%config) {
        # check that the given name is valid before blessing
        if $name ne ('A'..'Z').any {
            die "Invalid code '$name' given for formatting code; must be a capital ASCII letter";
        } elsif $name eq 'M' {
            die "Code 'M' passed to Pod6::FormatCode; please use Pod6::FormatCode::M instead";
        } elsif $name eq <F G H J O Q W Y>.any {
            die "Reserved code '$name' passed; if you're trying to extend formatting codes, do so through M<>";
        }

        self.bless(:$name, :@children, :%config);
    }

    multi method gist(Pod6::FormatCode:D:) {
        "{self.^name} $.name";
    }
}

#| The 'M' formatting code; M<foo:bar> becomes
#| Pod6::FormatCode::M::foo.new("bar")
class Pod6::FormatCode::M is Pod6::FormatCode {
    method new(*@children, *%config) {
        self.bless(:@children, :%config);
    }

    method name { "M" }
}

#| A block, such as =head1 or =SYNOPSIS
class Pod6::Block is Pod6::Excerpt does Pod6::Children {
    has Str $.name;

    method new(:$name, *@children, *%config) {
        if $name ne ($name.uc|$name.lc) {
            die "Mixed-case name used in Pod6::Block; please use Pod6::Block::M::$name if you meant that";
        }

        self.bless(:$name, :@children, :%config);
    }

    multi method gist(Pod6::Block:D:) {
        "{self.^name} $.name"
    }

    method level {
        if self<level>:exists {
            self<level>
        } else {
            warn "This block does not have a level associated with it.";
            0
        }
    }
}

#| A custom block, =for Foo is Pod6::Block::M::Foo
class Pod6::Block::M is Pod6::Block {
    method new(*@children, *%config) {
        self.bless(:@children, :%config);
    }

    method name { die "Unspecified Pod6::Block::M used (of type {self.^name}) -- did you forget to define .name?" }
}