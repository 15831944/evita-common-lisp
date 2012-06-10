#//
#// perl2lisp - Generate Perl test case for Common Lisp
#// perl2lisp.pl
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

die "Usage: $0: in [out]\n" unless 0 == $#ARGV || 0 == $#ARGV;

$in = $ARGV[0];

if (0 == $#ARGV)
{
    $out = ($in =~ m!([^/\\]+)$!)[0];
    $out =~ s/^retest-//;
    $out =~ s/^regex-test-//;
    $out =~ s/[.].*$//;
    $out = "$out.retest";
}
else
{
    $out = $ARGV[1];
}

open(IN,  $in) || die "Can't open $in";
open(OUT, ">$out") || die "Can't create $out";

$File = ($out =~ m!([^/\\]+)$!)[0];
($Prefix = $File) =~ s/[.].*$//;

&emit_header();
&main();

#//////////////////////////////////////////////////////////////////////
#//
#// main
#//
sub main()
{
    local($line) = 0;

    $bang = sprintf "\\%03o", ord "!"; # \41 would not be portable.
    $ffff  = chr(0xff) x 2;
    $nulnul = "\0" x 2;

    while (<IN>)
    {
        $line++;

        chomp;

        s! +$!!;

        if (m!^$!)  #// empty line
        {
            print OUT "\n";
            next;
        }

        if (m!^#!)  #// comment
        {
            next if 1 == $line;

            s/^#\s*//;
            print OUT ";" x 70, "\n";
            print OUT ";;;\n";
            print OUT ";;; $_\n";
            print OUT ";;;\n\n";
            next;
        }

        local($patsrc, $text, $repl, $replacement, $expect, $comment) =
            split("\t", $_, 6);

        $comment = "    ; $comment" if '' ne $comment;;


        local($pat)   = $patsrc;
        local($quote) = $patsrc =~ m/\\Q/ ? '"' : "'";
        local($mods)  = "";

        if ($patsrc =~ m|^([\x27:])(.*)\1(.*)$|)
        {
            $quote = $1;
            $pat   = $2;
            $mods  = $3;
        }

        if ($pat eq "''")
        {
            print "$File($line): empty pattern?";
            exit;
        }

        $pat  =~ s/(\$\{\w+\})/$1/eeg;
        $pat  =~ s/\\n/\n/g;

        $text =~ s/(\$\{\w+\})/$1/eeg;
        $text =~ s/\\n/\n/g;

        local($re) = eval("qr$quote$pat$quote$mods");
        local($error) = $@;

        if ("c" ne $repl)
        {
            if ($text =~ m/$re/)
            {
                @s = @-;
                @e = @+;
            }
            else
            {
                $#s = -1;
                $#e = -1;
            }
        }

        print OUT ";;;; $File/$line: $patsrc\n";
        print OUT ";;;\n";
        print OUT ";;;    ";
            if ($repl =~ m![Bb]!)
            {
                print OUT "Expect is NOT true due to bug of Perl.";
            }
            elsif ("c" eq $repl)
            {
                print OUT "Syntax error.";
            }
            elsif (-1 == $#s)
            {
                print OUT "Not mached.";
            }
            elsif (0 == $#s)
            {
                print OUT "Matched.";
            }
            else
            {
                print OUT "Matched with $#s captures.";
            }
            print OUT " ($repl)\n";
        print OUT ";\n";
        print OUT "(test-case$comment\n";
        print OUT "  \"$Prefix/$line\"\n";
        print OUT "  ", &lispfy($pat), "\n";
        print OUT "  ", &lispfy($text), "\n";

        #// options
        {
            local($separator) = "  '(";

            foreach $mod (split('', $mods))
            {
                print OUT "$separator";

                if ('i' eq $mod)
                {
                    print OUT ":ignore-case t";
                }
                elsif ('m' eq $mod)
                {
                    print OUT ":multiple-lines t";
                }
                elsif ('x' eq $mod)
                {
                    print OUT ":extended-syntax t";
                }
                else
                {
                    print STDERR "$File($line): Unknown modifier $mod\n";
                    exit 1;
                    #// NOTREACHED
                }

                $separator = " ";
            } #// foreach $mod

            if (index($mods, "s") < 0)
            {
                print OUT "$separator:single-line nil";
            }

            print OUT ")\n";
        } #// option

        if ("c" ne $repl)
        {
            &emit_result();
        }
        else
        {
            if ('' eq $error)
            {
                $error = "But Perl accepts this.";
            }
            else
            {
                $error =~ chomp($error);
                $error =~ s/\n/ /g;
                $error =~ s/\s+at \(re_eval \d+\) line \d+//g;
                $error =~ s/\s+in regex; marked by <--.*$//;
                $error =~ s/\s+at \(eval \d+\) line \d+, <IN> line \d+[.]//g;
                if (length($error) >= 700)
                {
                    $error = substr($error, 0, 60) + " ...";
                }
            }

            print OUT "  :compile   ; $error\n";
        }

        print OUT " )\n\n";
    } #// while

    print OUT "; EOF\n";
} #// main


#//////////////////////////////////////////////////////////////////////
#//
#// emit_result
#//
sub emit_result()
{
    if ($#s < 0)
    {
        print OUT "  nil";
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
} #// emit_result


#// escape
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
;;; Regex test case $File from Perl.
;;;
;;; This file is automatically generated from Perl's test data with
;;; Perl $] at $now.
;;;
;;; See: http://www.perl.com/ for Perl.
;;;
;

EOF
}
