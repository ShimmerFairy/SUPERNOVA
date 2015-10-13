# Pod6.pm6 --- various Pod6 classes for holding stuff

#| The base class of Pod6 stuff. Supports only per-block configuration stuff
#| (general =config statements are separate from this)
class Pod6::Excerpt does Associative {
    has %.config;

    method new(:%config) {
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
class Pod6::Text is Pod6::Excerpt { }

#| plain text in a block
class Pod6::Text::Plain is Pod6::Text does Pod6::Children[Str] {
    method new(:%config, *@children) {
        self.bless(:@children, :%config);
    }

    multi method gist(Pod6::Text:D:) {
        self.text.perl
    }

    method text { [~] @!children }

    multi method append(Str $new) { @!children.push($new) }
    multi method append(Pod6::Text::Plain $new) { @!children.append($new.list) }
}

#| base role for formatting codes
role Pod6::Text::FormatCode is Pod6::Text does Pod6::Children[Pod6::Text] {
    method new(:%config, **@children) {
        self.bless(:@children, :%config);
    }
    method preserves-spaces { False }
    method verbatim-text { False }

    method allow-fc($fc) { !self.verbatim-text || ?self<allow>.grep($fc) }

    method text { [~] self.list».text }

    multi method append(Str $new) { self.push(Pod6::Text::Plain.new($new)) }
    multi method append(Pod6::Text $new) { self.push($new) }

    multi method push(Str $new) { self.push(Pod6::Text::Plain.new($new)) }
}

#| role for reserved codes
role Pod6::Text::FormatCode::Reserved {
    method FALLBACK(|) {
        die "This formatting code ({self.^name}) is reserved"
    }
}

class Pod6::Text::FormatCode::A does Pod6::Text::FormatCode { }
class Pod6::Text::FormatCode::B does Pod6::Text::FormatCode { }

class Pod6::Text::FormatCode::C does Pod6::Text::FormatCode {
    method preserves-spaces { True }
    method verbatim-text { True }
}

class Pod6::Text::FormatCode::D does Pod6::Text::FormatCode {
    has $.term;
    has @.synonyms;

    method set-term($t) { $!term = $t.trim }
    method set-synonyms(@s) { @!synonyms = @s».trim }
}

class Pod6::Text::FormatCode::E does Pod6::Text::FormatCode {
    has $.text;

    method new($text) {
        self.bless(:$text);
    }

    # XXX apparently declaring a public attribute isn't enough when doing a role
    # that has this method. (Still declared public because that's what we want,
    # and to avoid needing a custom BUILD).
    method text { $!text }
}

class Pod6::Text::FormatCode::F does Pod6::Text::FormatCode::Reserved { }
class Pod6::Text::FormatCode::G does Pod6::Text::FormatCode::Reserved { }
class Pod6::Text::FormatCode::H does Pod6::Text::FormatCode::Reserved { }
class Pod6::Text::FormatCode::I does Pod6::Text::FormatCode { }
class Pod6::Text::FormatCode::J does Pod6::Text::FormatCode::Reserved { }

class Pod6::Text::FormatCode::K does Pod6::Text::FormatCode {
    method preserves-spaces { True }
}

class Pod6::Text::FormatCode::L does Pod6::Text::FormatCode {
    has $.address;

    method new($address) { self.bless(:$address) }

    method scheme { fail "No scheme set for {self.^name}" }
    method link { self.scheme ~ ':' ~ self.address }
}

class Pod6::Text::FormatCode::M does Pod6::Text::FormatCode {
    # in .verbatim, always get _this_ class' version of .text, which is the
    # default .text for Pod6::Text types
    method verbatim { self.Pod6::Text::FormatCode::M::text }

    method verbatim-text { True }
}

class Pod6::Text::FormatCode::N does Pod6::Text::FormatCode { }
class Pod6::Text::FormatCode::O does Pod6::Text::FormatCode::Reserved { }

class Pod6::Text::FormatCode::P does Pod6::Text::FormatCode {
    has $.address;

    method new($address) { self.bless(:$address) }

    method scheme { fail "No scheme set for {self.^name}" }
    method link { self.scheme ~ ':' ~ self.address }

    method text { die "Can't reproduce text yet" }
}

class Pod6::Text::FormatCode::Q does Pod6::Text::FormatCode::Reserved { }
class Pod6::Text::FormatCode::R does Pod6::Text::FormatCode { }

class Pod6::Text::FormatCode::S does Pod6::Text::FormatCode {
    method preserves-spaces { True }
}

class Pod6::Text::FormatCode::T does Pod6::Text::FormatCode {
    method preserves-spaces { True }
}

class Pod6::Text::FormatCode::U does Pod6::Text::FormatCode { }

class Pod6::Text::FormatCode::V does Pod6::Text::FormatCode {
    method verbatim-text { True }
}

class Pod6::Text::FormatCode::W does Pod6::Text::FormatCode::Reserved { }

class Pod6::Text::FormatCode::X does Pod6::Text::FormatCode {
    has %.entries;

    method new(@entries, **@children) {
        my %ent;

        for @entries {
            if $_ ~~ Pair {
                my $cur_e := %ent;
                my $p = $_;

                while $p !=== 1 {
                    $cur_e := $cur_e{$p.key.trim};
                    $p = $p.value;
                }

                $cur_e = 1;
            } else {
                %ent{$_.trim} = 1;
            }
        }

        self.bless(:entries(%ent), :@children);
    }
}

class Pod6::Text::FormatCode::Y does Pod6::Text::FormatCode::Reserved { }

class Pod6::Text::FormatCode::Z does Pod6::Text::FormatCode {
    method text { "" }
    method hidden { self.Pod6::Text::FormatCode::text }
}

#### Specialized L<> classes

class Pod6::Text::FormatCode::L::Http is Pod6::Text::FormatCode::L {
    has $.internal = "";
    has $.external;

    method new($address) {
        my $internal;
        my $external;

        ($external, $internal) = $address.split('#', 2);
        $external.=trim.subst(/\s+/," ", :g);
        $internal.=trim.subst(/\s+/," ", :g);

        self.bless(:$internal, :$external);
    }

    method scheme { "http" }
    method address { self.external ~ ('#' ~ self.internal if self.internal) }
}

class Pod6::Text::FormatCode::L::Doc is Pod6::Text::FormatCode::L {
    has $.doc;
    has $.inner-by = "onehead";
    has @.inner;

    # based on conjectural internal addressing
    method new($address) {
        my $doc;
        my $inner-by;
        my @inner;

        my @p = $address.split("#");

        $doc = @p.shift;
        $inner-by = @p.shift.trim.subst(/\s+/," ", :g) if +@p > 1;
        @inner = @p.map: *.trim.subst(/\s+/, " ", :g);

        self.bless(:$address, :$doc, :$inner-by, :@inner);
    }

    method scheme { "doc" }
}

#### Specialized P<> classes

class Pod6::Text::FormatCode::P::Http is Pod6::Text::FormatCode::P {
    has $.internal = "";
    has $.external;

    method new($address) {
        my $internal;
        my $external;
        ($external, $internal) = $address.split('#', 2);
        $external.=trim.subst(/\s+/," ", :g);
        $internal.=trim.subst(/\s+/," ", :g);

        self.bless(:$internal, :$external);
    }

    method scheme { "http" }
    method address { self.external ~ ('#' ~ self.internal if self.internal) }
}

class Pod6::Text::FormatCode::P::Doc is Pod6::Text::FormatCode::P {
    has $.doc;
    has $.inner-by = "onehead";
    has @.inner;

    # based on conjectural internal addressing
    method new($address) {
        my $doc;
        my $inner-by;
        my @inner;

        my @p = $address.split("#");

        $doc = @p.shift;
        $inner-by = @p.shift.trim.subst(/\s+/," ", :g) if +@p > 1;
        @inner = @p.map: *.trim.subst(/\s+/, " ", :g);

        self.bless(:$address, :$doc, :$inner-by, :@inner);
    }

    method scheme { "doc" }
}

class Pod6::Config { ... }

#| base class for blocks
class Pod6::Block is Pod6::Excerpt does Pod6::Children {
    # these are numeric margins; character margins are in the config options
    has $.vmargin = 0;
    has $.cvmargin = 0;

    method new(:%config, *@children) {
        self.bless(:@children, :%config);
    }

    method implies-code { False }
    method implies-para { False }

    method preserves-spaces { False }
    method verbatim-text { False }

    method allow-fc($fc) { !self.verbatim-text || ?self<allow>.grep($fc) }

    method set-vmargin(UInt $spaces = 0)  { $!vmargin  = $spaces }
    method set-cvmargin(UInt $spaces = 0) { $!cvmargin = $spaces }

    # XXX currently just does space-related
    method get-margin { $!vmargin }
    method get-para-margin { self.get-margin() } # XXX will differ with :margin support
    method get-code-margin { $!cvmargin }

    method last-config-block { self.list.grep(* ~~ Pod6::Config)[*-1] }

    #| gets the contents of the block as plaintext. Meant for "bottom" blocks
    #| (which only contain text, and not other blocks), but works on all blocks.
    method text { [~] @!children».text }
}

class Pod6::Block::Code is Pod6::Block {
    method preserves-spaces { True }
    method verbatim-text { True }
}
class Pod6::Block::Comment is Pod6::Block { }

class Pod6::Block::Defn is Pod6::Block {
    has $.term;

    method new(:%config, :$term, *@children) {
        self.bless(:$term, :%config, :@children);
    }
}

class Pod6::Block::Head is Pod6::Block {
    has $.level;

    method new(:$level!, :%config, *@children) {
        self.bless(:$level, :@children, :%config);
    }
}

class Pod6::Block::Input is Pod6::Block {
    method preserves-spaces { True }
}

class Pod6::Block::Item is Pod6::Block {
    has $.level;

    method new(:$level = 1, :%config, *@children) {
        self.bless(:$level, :@children, :%config);
    }

    method implies-code { True }
    method implies-para { True }
}

class Pod6::Block::Nested is Pod6::Block {
    method implies-code { True }
    method implies-para { True }
}

class Pod6::Block::Output is Pod6::Block {
    method preserves-spaces { True }
}

class Pod6::Block::Para is Pod6::Block { }

class Pod6::Block::Pod is Pod6::Block {
    method implies-code { True }
    method implies-para { True }
}
class Pod6::Block::Table is Pod6::Block { } # XXX replace with actual table class

class Pod6::Block::Data is Pod6::Block {
    method preserves-spaces { True }
}

class Pod6::Block::Finish is Pod6::Block {
    method implies-code { True }
    method implies-para { True }
}

#| A semantic block (XXX maybe we want these separated by class, but we'd really
#| want a macro to make all those classes for us, since they all behave
#| fundamentally the same)
class Pod6::Block::SEMANTIC is Pod6::Block {
    has $.name;

    method new(:$name, :%config, *@children) {
        self.bless(:$name, :@children, :%config);
    }

    multi method gist(Pod6::Block::SEMANTIC:D:) {
        my $gist = "Pod6::Block::SEMANTIC - $!name\n";

        if self.gist-config {
            $gist ~= "Configuration:\n".indent(2);
            $gist ~= self.gist-config.indent(4) ~ "\n";
        }

        if self.gist-children {
            $gist ~= "Children:\n".indent(2);
            $gist ~= self.gist-children.indent(4) ~ "\n";
        }

        $gist.chomp;
    }

    method implies-code { True }
    method implies-para { True }
}

#| A custom block (isn't Pod6::Block, that's for standard types)
class Pod6::MBlock is Pod6::Excerpt does Pod6::Children {
    method new(:%config, *@children) {
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

    # for Pod6::Block.text
    method text { "" }
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