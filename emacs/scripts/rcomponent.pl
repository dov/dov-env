#!/usr/bin/perl -w

# This script contains perl code for doing various code conversions
# and instantiations related to the XJet RComponent library.

use strict;

# Split on outer comma, allow for matching parens
sub encode_comma {
    my $v = shift;
    $v =~ s/,/<<<comma>>>/g;
    return $v;
}

sub decode_comma {
    my $v = shift;
    $v =~ s/<<<comma>>>/,/g;
    return $v;
}

# Split, but ignore commas in matching pars. Needed for template
# matching. Note only supports single nesting.
sub psplit {
    my $v = shift;
    # replace comma in matching pars with an arbirtrary string
    $v =~ s/\((.*?)\)/"(" . encode_comma($1) . ")"/ge;

    # now split.
    my @v = split(/,\s*/,$v);

    # decode comma
    return map { decode_comma($_) }  @v;
}

# Output a skeleton C function
sub PrintCFunction {
    my($comment, $Ret, $ClassName, $fnc, $args) = @_;
    my $args_string = join(", ", @$args);
    if (@$args == 0) {
        $args_string = 'void';
    }
    my $t;
    $t .= "$comment\n" if $comment;
    $t .= "$Ret ${ClassName}::${fnc}($args_string)\n"
        ."{\n"
        ."  R_TRACE;\n"
        ."\n"
        ."  // TBD\n"
        ."}\n\n";
    return $t;
}

# Convert def statements to init statements
sub Def2Init {
    my $text = shift;
    my $ClassName = shift;
    # states
    #  0 - Before comment
    #  1 - In comment
    #  2 - In function
    
    my $state = 0;
    my $comment = '';
    my $t; #output text
    for (split("\n", $text)) {
        if (m://\s*(.*):) {
            if ($state == 0) {
                $state = 1;
                $comment = $1;
            }
            else {
                $comment .= " $1";
            }
        }
        elsif (m/(DEFINE.*?)\((.*)\)/) {
            $state = 0;
            my $fnc = $1;
            my $args = $2;

            # Get rid of matching pars
            my @args = psplit($args);
    
            # Build doc string
            my $class = shift(@args);
            if ($fnc !~ /VOID/) {
                shift(@args);
            }
    
            my $fnc_name = shift @args;
            my @vars = ();
            for my $v (@args)
            {
                if ($v=~ m:/\*(.*)\*/:) {
                    push(@vars, $1);
                }
            }
            my $varstring = join(",", @vars);
    
    #        print "DEFINE: $fnc_name Vars = ", join(",",@vars), "\n";
            $t.= "  INIT_METHOD_DOC($ClassName,$fnc_name,\"$fnc_name($varstring): $comment\");\n";
        }
    }
    return $t;
}

# Convert H declarations to C statements.
sub H2C {
    my $text = shift;
    my $ClassName = shift;

    my $ret = '';
    while($text=~ m/\s*(\w+)\s+(\w+)\s*\((.*?)\)\s*;/sg)
    {
        my $return = $1;
        my $fnc = $2;
        my $args = $3;

        # Print everything from the last match
        my $comment = $`;
        $comment=~ s/^\s*//gm;

        my @args = psplit($args);

        $ret .= PrintCFunction($comment, $return, $ClassName, $fnc, \@args);
    }
    return $ret;
}

# Convert H declarations to Def statements.
# TBD - support virtual functions
sub H2Def {
    my $text = shift;
    my $ClassName = shift;

    my $ret = '';
    while($text=~ m/(\w+)\s+(\w+)\s*\((.*?)\)\s*;/sg)
    {
        my $Return = $1;
        my $fnc = $2;
        my $args = $3;

        my @args = psplit($args);

        # comment last arg
        map { s:(\S+\s+)(\w+)$:$1/\*$2\*/: } @args;
        my $arg_string = join(",", @args);
        my $NumArgs = scalar(@args);
        my $VoidArgs = ($NumArgs == 0 or $args[0] eq 'void');

        if ($Return=~ /void/ and $VoidArgs) {
            $ret .= "    DEFINE_VOID_METHOD($ClassName,$fnc);\n";
        }
        elsif ($Return=~ /void/) {
            $ret .= "    DEFINE_VOID_METHOD_${NumArgs}($ClassName,$fnc,$arg_string);\n";
        }
        elsif ($VoidArgs) {
            $ret .= "    DEFINE_METHOD($ClassName,$Return,$fnc);\n";
        }
        else {
            $ret .= "    DEFINE_METHOD_${NumArgs}($ClassName,$Return,$fnc,$arg_string);\n";
        }
    }
    return $ret;
}

# Convert Def statements to C template code
sub Def2C {
    my $text = shift;
    my $ClassName = shift;
    
    my $t = '';   # Returned text
    for (split("\n", $text)) {

        # Unindent and print comments
        if (s:^\s*//://:) {
            $t .= "$_\n";
            next;
        }
        elsif (m/(DEFINE.*?)\((.*)\)/) {
            my $rfnc = $1;
            my $args = $2;

            # Get rid of matching pars
            my @args = psplit($args);

            my $s = '';

            my $ret;
            my $cn = shift(@args); # ClassName
            if ($rfnc=~ /VOID/) {
                $ret = 'void';
            }
            else {
                $ret = shift(@args);
            }
            my $fnc = shift(@args);

            # Get rid of comments around arguments
            map { s:\s*/\*\s*: :;  s:\*/::} @args;

            $t .= PrintCFunction('',$ret,$ClassName,$fnc,\@args);
        }
    }
    return $t;
};

my $def2init = 0;
my $h2def=0;
my $def2c = 0;
my $h2c = 0;
my $ClassName;

while(@ARGV && ($_ = $ARGV[0], /^-/)) {
    shift;
    /^-ClassName/ and do {
        $ClassName = shift;
        next;
    };
    /^-def2init/ and do { $def2init=1; next; };
    /^-def2c/ and do { $def2c=1; next; };
    /^-h2c/ and do { $h2c=1; next; };
    /^-h2def/ and do { $h2def=1; next; };
    /^-help/ and do {
        print <<__;
rcomponent.pl -- Convert code for the rcomponent library

Syntax:
    rcomponent.pl -ClassName cn [-h2def] [-def2h] [-def2init]
__
    };
    die "Unknown option $_!\n";
}

my $text = join("",<>);

if ($def2init) {
    print Def2Init($text, $ClassName);
}
elsif ($h2def) {
    print H2Def($text, $ClassName);
}
elsif ($h2c) {
    print H2C($text, $ClassName);
}
elsif ($def2c) {
    print Def2C($text, $ClassName);
}
