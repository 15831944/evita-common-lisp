#//
#// pcre2lisp - Generate PCRE test case for Common Lisp
#// pcre2lisp.pl
#//
#// This file is NOT part of Evita Common Lisp.
#//
#// Copyright (C) 1996-2004 by Project Vogue.
#// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#//
#// @(#)$Id: /proj/evcl/lisp/regex/test/regex-test.lisp 11 2004-06-20 06:27:43 yosi $
#//
#// Description:
#//  Generates test case for Regex facility of Evita Common Lisp from PCRE
#//  test case.
#//
#// See http://www.pcre.org/ for PCRE, Perl Compatible Regex.
#//

#// CALL_ARGUMENTS_LIMIT
#//
#// Description:
#//   Test driver takes matched string and captured string in list. So, test
#//   driver can handler at most $CALL_ARGUMENTS_LIMIT - 1 captures.
#//
$CALL_ARGUMENTS_LIMIT = 30;

die "Usage: $0: in out\n" unless 1 == $#ARGV;

$in = $ARGV[0];
$out = $ARGV[1];

open(IN,  $in) || die "Can't open $ARGV[0]";
open(OUT, ">$out") || die "Can't create $ARGV[1]";

$File = ($in  =~ m!([^/\\]+)$!)[0];
$Name = ($out =~ m!([^/\\]+)$!)[0];
($Prefix = $Name) =~ s/[.].*$//;

&emit_header();
&main();

#//////////////////////////////////////////////////////////////////////
#//
#// main
#//
sub main()
{
    local($expr) = '';
    local($line) = 0;
    local($ready) = 0;

    while (<IN>)
    {
        $line++;

        s! +$!!;

        if (m!^\s*$!)       #// empty line
        {
            if ($ready || '' eq $expr)
            {
                $expr = '';
                $ready = 0;
                next;
            }
        }

        if ($ready)
        {
            if (m!^\s!)
            {
                s/^\s*//;
                chomp;

                if (length($expr) < 200000)
                {
                    &process($expr, $_);
                }
                else
                {
                    print "$File($line): WARN: Ignore long pattern: ",
                        length($expr), "\n";
                }
            }
            else
            {
                # comment
            }
        }
        else #// Pattern
        {
            if ($ready)
            {
                print "name($line): Unexpected: expr=|$expr|\n";
                exit 1;
                $expr = '';
            }

            $expr .= $_;
            $ready = ($expr =~ m!^(.).*\1.*$!s);

            if ($ready)
            {
                chomp($expr);
            }
        }
    } #// while

    print OUT "; EOF\n";
} #// main


#//////////////////////////////////////////////////////////////////////
#//
#// process
#//
sub process()
{
    local($expr, $input) = @_;
    local($text) = eval(qq|"$input"|);


    {
        local($header) = ";;;; $File/$line: ", &escape($expr);
        $header =~ s/\n/\\n/g;
        if (length($header) >= 70)
        {
            $header = substr($header, 0, 69) . "...";
        }

        print OUT "$header\n";
        print OUT ";;;\n";
    }

    local($quote, $patsrc, $suffix) = ($expr =~ m!^(.)(.*)\1(.*)$!s);

    if ('' eq $patsrc)
    {
        $patsrc = "(?#)";

        if ($expr ne '//g')
        {
            print OUT "\n";
            print "$File($line): Failed to decompose: |$expr|\n";
            #// exit 1;
            return;
        }
    }

    local($mods) = $suffix;
        $mods =~ s![^imsx]!!g;

    local($loop) = index($suffix, 'g') >= 0 ? "/g" : "";

    local($pattern) = $patsrc;
    {

        #// $pattern = eval("qq${quote}${patsrc}${quote}");

        #// Replace \xX to \x0X, since we don't like it.
        if ($pattern =~ /x[0-9a-fA-F](?![0-9a-fA-F])/)
        {
            print "$File($line): FIX: \\xX => \xXX: $pattern\n";
            $pattern =~ s/\\x([0-9a-fA-F])(?![0-9a-fA-F])/\\x0$1/g;
        }

        #// Replace \" to ", since we don't like it.
        if ($pattern =~ /(?<!\\)\\\x22/)
        {
            print "$File($line): FIX: \\\x22 => \x22: $pattern\n";
            $pattern =~ s/\\\x22/\x22/g;
        }

        #// Replace \' to ', since we don't like it.
        if ($pattern =~ /(?<!\\)\\\x27/)
        {
            print "$File($line): FIX: \\\x27 => \x27: $pattern\n";
            $pattern =~ s/\\\x27/\x27/g;
        }

        #// Replace \@ to @, since we don't like it. (testdata1/1566)
        if ($pattern =~ /(?<!\\)\\\x40/)
        {
            print "$File($line): FIX: \\\x40 => \x40: $pattern\n";
            $pattern =~ s/\\\x40/\x40/g;
        }

        #// Replace \/ to /, since we don't like it. (testdata1/1566)
        if ($pattern =~ /(?<!\\)\\\x2F/)
        {
            print "$File($line): FIX: \\\x2F => \x2F: $pattern\n";
            $pattern =~ s/\\\x2F/\x2F/g;
        }


        #// Replace \[\] to \x5B\x5D, since we don't like it.
        if ($pattern =~ m!\\\[\\\]!x)
        {
            # print "$File($line): FIX: \\\x5B => \x5B: $pattern\n";
            $pattern =~ s!\\\[\\\]!\x5Cx5B\x5Cx5B!g;
        }

    } #// $pattern

    #// Remove named capture
    $patsrc =~ s!\x28[?]P<[^>]+>!\x28!g;  #// (?P<name>...) => (...)

    local($error) = "";
    local($re) = eval("qr${quote}${patsrc}${quote}$mods");
        if ($@)
        {
            $error = "Perl failed to compile.";
        }

    print OUT ";;;    ";
    if ("" ne $error)
    {
        #// nothing to do
    }
    elsif (! $loop)
    {
        local($matched_p);
        local($ngroups);
        {
            #// I'm not sure why qr/.../m doesn't work.
            if ($mods =~ /m/)
            {
                $matched_p = $text =~ m/$re/m;
                $ngroups = $#-;
            }
            else
            {
                $matched_p = $text =~ m/$re/;
                $ngroups = $#-;

            }
        }

        if (! $matched_p)
        {
            print OUT "Not matched";
        }
        elsif (0 == $ngroups)
        {
            print OUT "Matched";
        }
        elsif ($ngroups + 1 < $CALL_ARGUMENTS_LIMIT)
        {
            print OUT "Matched with $ngroups captures.";
        }
        else
        {
            print OUT "Matched with $ngroups captures.\n";
            print OUT "; Too many captures for test driver.\n";

            print "$File($line): Too many captures for test driver.\n";
            return;
        }
    } # if ! $loop

    print OUT "\n;\n";

    #// (?P<name>...) => (?<name>...)
    $pattern =~ s!\x28[?]P(<[^>]+>)!\x28?$1!g;

    #// (?P=<name>) => \k<name>
    $pattern =~ s!\x28[?]P=([^\x29]+)\x29!\x5ck<$1>!g;

    print OUT "(test-case$loop\n";
    print OUT "  \"$Prefix/$line\"\n";
    print OUT "  ", &lispfy($pattern), "\n";
    print OUT "  ", &lispfy($text), "\n";

    {
        print OUT "  '";

        local($separator) = "(";
        foreach $mod (split('', $suffix))
        {
            next if "g" eq $mod;
            next if "+" eq $mod;

            local($key);

            if ("i" eq $mod)
            {
                $key = "ignore-case";
            }
            elsif ("m" eq $mod)
            {
                $key = "multiple-lines";
            }
            elsif ("s" eq $mod)
            {
                $key = "single-line";
            }
            elsif ("x" eq $mod)
            {
                $key = "extended-syntax";
            }
            elsif ("8" eq $mod)
            {
                $key = "unicode";
            }
            else
            {
                print "$File($line): Unknown modifier: $mod\n";
                next;
            }

            print OUT "$separator:$key t";
            $separator = " ";
        } # for each $mod

        if (index($mods, "s") < 0)
        {
            print OUT "${separator}:single-line nil";
        }

        print OUT ")\n";
    }

    if ("" ne $error)
    {
        print OUT "   :compile  ; $error\n";
    }
    elsif ("" ne $loop)
    {
        print OUT "  (list";

        local($count) = 0;

        while ($text =~ /$re/g)
        {
            if (0 == $count)
            {
                print OUT " ";
            }
            else
            {
                print OUT "\n        ";
            }

            $count++;

            print OUT &lispfy($&);
        } # while

        print OUT " " if $count >= 2;
        print OUT ")\n";

        print OUT "    ", &lispfy($POSTMATCH);
    }
    else
    {

        local($matched_p);
        local(@s);
        local(@e);
        {
            #// I'm not sure why qr/.../m doesn't work.
            if (index($mods, "m") >= 0)
            {
                $matched_p = $text =~ m/$re/m;
                @s = @-;
                @e = @+;

            }
            else
            {
                $matched_p = $text =~ m/$re/;
                @s = @-;
                @e = @+;
            }
        }

        if (! $matched_p)
        {
            print OUT "  nil\n";
        }
        else
        {
            print OUT "  (list";

            for ($i = 0; $i <= $#s; $i++)
            {
                if (0 == $#s)
                {
                    print OUT " ";
                }
                else
                {
                    print OUT "\n    ";
                }

                if (!defined($s[$i]))
                {
                    print OUT "nil";
                }
                else
                {
                    local($substr) = substr($text, $s[$i], $e[$i] - $s[$i]);
                    print OUT &lispfy($substr);
                }
            } # for $i

            print OUT " " if $#s >= 1;
            print OUT ")";
        }
    }

    print OUT " )\n\n";
} # process


# escape
sub escape($)
{
    local($string) = @_;

    if ($string =~ /^[\x20-\x7E]*$/)
    {
        return $string;
    }

    local($result) = "";
    foreach $code (unpack("U*", $string))
    {
        if ($code >= 0x20 && $code <= 0x7E)
        {
            $result .= chr($code);
        }
        else
        {
            $result .= sprintf("\\u%04X", $code);
        }
    } # foreach

    return $result;
} # escape


# lispfy
#  Returns lisp form which represents specified string. If string contains
#  only graphic characters, result is double quoted string, otherwise
#  returns coerce form.
sub lispfy($)
{
    local($string) = @_;

    if ($string =~ /^[\x20-\x7E]*$/)
    {
        $string =~ s!\x5C!\x5C\x5C!g;    # escape backslash
        $string =~ s!"!\\"!g;           # escape double quote
        return qq|"$string"|;
    }

    local($result) = "";

    foreach $code (unpack("U*", $string))
    {
        if (0x22 == $code || 0x5C == $code)
        {
            $result .= "\\" . chr($code);
            next;
        }

        if ($code >= 0x20 && $code <= 0x7E)
        {
            $result .= chr($code);
            next;
        }
        elsif (0x0A == $code)
        {
            $result .= '\\\\n';
        }
        elsif (0x09 == $code)
        {
            $result .= '\\\\t';
        }
        else
        {
            $result .= sprintf("\\\\u%04X", $code);
        }
    } #// foreach $code
    return '(backslash "' . $result . '")';
} # lispfy


sub emit_header
{
    local($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = gmtime;

    local($now) = sprintf("%04d-%02d-%02d %02d:%02d:%02d UTC",
        $year + 1900,
        $mon,
        $mday,
        $hour,
        $sec,
        $min );

    print OUT <<EOF;
;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; Base: 10 -*-
;;; Regex test case $File from PCRE.
;;;
;;; This file is automatically generated from PCRE's test data with
;;; Perl $] at $now.
;;;
;;; See: http://www.pcre.org/ for PCRE, Perl Compatible Regex.
;;;
;

EOF
}
