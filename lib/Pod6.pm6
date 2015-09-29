# Pod6.pm6 --- various Pod6 classes for holding stuff

#| The base class of Pod6 stuff. Supports just configuration stuff
class Pod6::Excerpt does Associative {
    has %.config;

    method new(*%config) {
        self.bless(:%config);
    }

    multi method hash(Pod6::Excerpt:D:)                          { %!config }
    multi method AT-KEY(Pod6::Excerpt:D: Str $key) is rw         { %!config.AT-KEY($key) }
    multi method EXISTS-KEY(Pod6::Excerpt:D: Str $key)           { %!config.EXISTS-KEY($key) }
    multi method DELETE-KEY(Pod6::Excerpt:D: Str $key)           { %!config.DELETE-KEY($key) }
    multi method ASSIGN-KEY(Pod6::Excerpt:D: Str $key, Str $val) { %!config.ASSIGN-KEY($key, $val) }

    multi method gist(Pod6::Excerpt:D:) {
        my $gist = "{self.^name}:\n";
        $gist ~= self.gist-children.indent(4);
        $gist.chomp;
    }

    method gist-children { "" }
}

#| a role for Pod6 classes having children
role Pod6::Children[::ELEMT = Pod6::Excerpt] does Positional {
    has @.children;

    multi method list(Pod6::Children:D:)                  { @!children }
    multi method elems(Pod6::Children:D:)                 { @!children.elems }
    multi method AT-POS(Pod6::Children:D: $idx) is rw     { @!children.AT-POS($idx) }
    multi method EXISTS-POS(Pod6::Children:D: $idx)       { @!children.EXISTS-POS($idx) }
    multi method DELETE-POS(Pod6::Children:D: $idx)       { @!children.DELETE-POS($idx) }
    multi method ASSIGN-POS(Pod6::Children:D: $idx, $val) { @!children.ASSIGN-POS($idx, $val) }
    multi method push(Pod6::Children:D: **@vals)          { die unless ELEMT ~~ @vals.all; @!children.push(|@vals) }

    method gist-children(Pod6::Children:D:) {
        my $childs = "";
        $childs ~= $_.gist ~ "\n" for @!children;
        $childs.chomp;
    }

    method give-config {
        @!children».take-config(self.hash);
    }

    method take-config(%inheritance) {
        for %inheritance.kv -> $k, $v {
            unless self{$k}:exists {
                self{$k} = $v
            }
        }
    }
}

#| text found in a block (just a base class to text types)
class Pod6::Text is Pod6::Excerpt {
    multi method gist(Pod6::Text:D:) {
        self.text.perl
    }
}

#| plain text in a block
class Pod6::Text::Plain is Pod6::Text does Pod6::Children[Str] {
    method new(*@children, *%config) {
        self.bless(:@children, :%config);
    }

    method text { [~] @!children }
}

#| base class for blocks
class Pod6::Block is Pod6::Excerpt does Pod6::Children {
    method new(*@children, *%config) {
        self.bless(:@children, :%config);
    }
}

class Pod6::Block::Code is Pod6::Block { }
class Pod6::Block::Comment is Pod6::Block { }
class Pod6::Block::Defn is Pod6::Block { }

class Pod6::Block::Head is Pod6::Block {
    has $.level;

    method new(:$level!, *@children, *%config) {
        self.bless(:$level, :@children, :%config);
    }
}

class Pod6::Block::Input is Pod6::Block { }

class Pod6::Block::Item is Pod6::Block {
    has $.level;

    method new(:$level = 1, *@children, *%config) {
        self.bless(:$level, :@children, :%config);
    }
}

class Pod6::Block::Nested is Pod6::Block { }
class Pod6::Block::Output is Pod6::Block { }
class Pod6::Block::Para is Pod6::Block { }
class Pod6::Block::Pod is Pod6::Block { }
class Pod6::Block::Table is Pod6::Block { } # XXX replace with actual table class
class Pod6::Block::Data is Pod6::Block { }
class Pod6::Block::Finish is Pod6::Block { }

class Pod6::Block::Ambient is Pod6::Block { }
class Pod6::Block::Declarator is Pod6::Block { }

#| A semantic block (XXX maybe we want these separated by class, but we'd really
#| want a macro to make all those classes for us, since they all behave
#| fundamentally the same)
class Pod6::Block::SEMANTIC is Pod6::Block {
    has $.name;

    method new(:$name, *@children, *%config) {
        self.bless(:$name, :@children, :%config);
    }

    multi method gist(Pod6::Block::SEMANTIC:D:) {
        my $gist = "Pod6::Block::SEMANTIC - $!name\n";
        $gist ~= self.gist-children().indent(4);
        $gist.chomp;
    }
}

#| A custom block (isn't Pod6::Block, that's for standard types)
class Pod6::MBlock is Pod6::Excerpt does Pod6::Children {
    method new(*@children, *%config) {
        self.bless(:@children, :%config);
    }

    method block-name { self.^name.split('::')[*-1] }
}
