# Pod6.pm6 --- various Pod6 classes for holding stuff

#| The base class of Pod6 stuff. Supports only per-block configuration stuff
#| (general =config statements are separate from this)
class Pod6::Excerpt does Associative {
    has %.config;

    method new(*%config) {
        self.bless(:%config);
    }

    multi method hash(Pod6::Excerpt:D:)                          { %!config }
    multi method AT-KEY(Pod6::Excerpt:D: Str $key) is raw        { %!config.AT-KEY($key) }
    multi method EXISTS-KEY(Pod6::Excerpt:D: Str $key)           { %!config.EXISTS-KEY($key) }
    multi method DELETE-KEY(Pod6::Excerpt:D: Str $key)           { %!config.DELETE-KEY($key) }
    multi method ASSIGN-KEY(Pod6::Excerpt:D: Str $key, Str $val) { %!config.ASSIGN-KEY($key, $val) }

    multi method gist(Pod6::Excerpt:D:) {
        my $gist = "{self.^name}:\n";

        if %!config {
            $gist ~= "Configuration:\n".indent(2);
            $gist ~= self.gist-config.indent(4) ~ "\n";
        }

        if self.gist-children {
            $gist ~= "Children:\n".indent(2);
            $gist ~= self.gist-children.indent(4);
        }

        $gist.chomp;
    }

    method set-config(%set-to) { %!config = %set-to }
    method add-to-config(%newopts) {
        for %newopts.kv -> $k, $v {
            %!config{$k} = $v;
        }
    }

    method gist-children { "" }

    method gist-config(Pod6::Excerpt:D:) {
        sub one-level(%h) {
            my $gist;
            for %h.kv -> $k, $v {
                $gist ~= "$k => ";

                if $v ~~ Associative {
                    $gist ~= "\{\n";
                    $gist ~= one-level($v).indent(2);
                    $gist ~= "}\n";
                } else {
                    $gist ~= $v.gist ~ "\n";
                }
            }
            $gist
        }

        one-level(%!config).chomp;
    }
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
    multi method push(Pod6::Children:D: *@vals) {
        die "{ELEMT.^name} vs {@vals.gist} -> {@vals.all ~~ ELEMT}" unless @vals.all ~~ ELEMT;
        @!children.push(|@vals)
    }

    method gist-children(Pod6::Children:D:) {
        my $childs = "";
        $childs ~= $_.gist ~ "\n" for @!children;
        $childs.chomp;
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

class Pod6::Config { ... }

#| base class for blocks
class Pod6::Block is Pod6::Excerpt does Pod6::Children {
    # these are numeric margins; character margins are in the config options
    has $.vmargin = 0;
    has $.cvmargin = 0;

    method new(*@children, *%config) {
        self.bless(:@children, :%config);
    }

    method implies-code { False }
    method implies-para { False }

    method set-vmargin(UInt $spaces = 0)  { $!vmargin  = $spaces }
    method set-cvmargin(UInt $spaces = 0) { $!cvmargin = $spaces }

    # XXX currently just does space-related
    method get-margin { $!vmargin }
    method get-para-margin { self.get-margin() } # XXX will differ with :margin support
    method get-code-margin { $!cvmargin }

    method last-config-block { self.list.grep(* ~~ Pod6::Config)[*-1] }
}

class Pod6::Block::Code is Pod6::Block { }
class Pod6::Block::Comment is Pod6::Block { }
class Pod6::Block::Defn is Pod6::Block {
    method implies-code { True }
    method implies-para { True }
}

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

    method implies-code { True }
    method implies-para { True }
}

class Pod6::Block::Nested is Pod6::Block {
    method implies-code { True }
    method implies-para { True }
}
class Pod6::Block::Output is Pod6::Block { }
class Pod6::Block::Para is Pod6::Block { }
class Pod6::Block::Pod is Pod6::Block {
    method implies-code { True }
    method implies-para { True }
}
class Pod6::Block::Table is Pod6::Block { } # XXX replace with actual table class
class Pod6::Block::Data is Pod6::Block { }
class Pod6::Block::Finish is Pod6::Block {
    method implies-code { True }
    method implies-para { True }
}

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

    method implies-code { True }
    method implies-para { True }
}

#| A custom block (isn't Pod6::Block, that's for standard types)
class Pod6::MBlock is Pod6::Excerpt does Pod6::Children {
    method new(*@children, *%config) {
        self.bless(:@children, :%config);
    }

    method block-name { self.^name.split('::')[*-1] }
}

#| Configuration options. Contains an accumulation of configurations, so you
#| only need to find the latest one. Additionally, each Pod6::Block starts with
#| a Config block, to make sure searching doesn't have to jump back out. This
#| uses the same config mechanism inherited from Pod6::Excerpt, but fills it
#| with a hash of hashes instead of the more normal one-level hash.
class Pod6::Config is Pod6::Excerpt {
    method new() { self.bless() }

    method add-config(Str $blorf, Str $key, $value) {
        self{$blorf}{$key} = $value;
    }

    method grab-config-for(Str $blorf) { self{$blorf} // Hash.new }

    method inherit-config(Pod6::Config:D $oldconfig) {
        for $oldconfig.hash.kv -> $blorf, $opts {
            for $opts.kv -> $key, $val {
                self.add-config($blorf, $key, $val);
            }
        }
    }

    has $.get-margin;
    method set-vmargin($a) { $!get-margin = $a }
}

#| The document class. The base container for every Pod construct in a file.
class Pod6::Document is Pod6::Block { }

#| Dummy classes, to be implemented
class Pod6::Ambient is Pod6::Excerpt { }
class Pod6::Declarator is Pod6::Excerpt  { }
class Pod6::Alias is Pod6::Excerpt { }
class Pod6::Encoding is Pod6::Excerpt {
    has $.get-margin;
    method set-vmargin($a) { $!get-margin = $a }
}