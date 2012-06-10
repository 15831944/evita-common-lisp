# &test('a)', 'a)');    # unmatched closed-parenthesis

# marked as b in re_tests
"abc" =~ /((a))/;
print "\@- =|@-|\n";
print "\@* =|@*|\n";

"abca" =~ /(?:(a)|(b)|(c))*/; print "s=|@-| e=|@*|\n";
"abcb" =~ /(?:(a)|(b)|(c))*/; print "s=|@-| e=|@*|\n";
"abcc" =~ /(?:(a)|(b)|(c))*/; print "s=|@-| e=|@*|\n";

# Capturing
&test("(foo|bar|x\\1)*", "fooxfoo");
&test("(foo|bar|x\\1)*", "barxbar");
&test("(foo|bar|x\\1)*", "x");          # match empty
&test("(foo|bar|x\\1)+", "x");          # not match
&test("((a{0,5})*)*[c]", "ac");


&test(".X((a*|b*|c*|d*)*)*X", "bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");

&test('(?:(a)(b))*x()', 'ababb x');     # 6   7/7   7
&test('(.*)(?=c)c', 'abcd');            # $1=ab marked as yB
&test('(.*)(?=[bc])c', 'abcd');         # $1=ab marked as yB
&test('(.*?)(?=c)c', 'abcd');           # $1=ab marked as yB
&test('(.*?)(?=[bc])c', 'abcd');        # $1=ab marked as yB

&test('(?:(a)|(b)|(c))*x()', 'c--abx'); # |3 3 4  6|6 4 5  6|
&test('^(a(b)?)*()$', 'ab');            # |0 0 1 2/ 2 2 2 2
&test('^(a(b)?)*()$', 'a');             # |0 0  1/1 1  1
&test("((a)|(b))*", "aabbaa");          # |0 5 5 3/6 6 6 4
&test('(a)|(b)', 'a');
&test('(a)|(b)', 'b');
&test('[\d]', 'd');     # check "\d" as 0-9
&test('[\d]', '9');     # check "\d" as 0-9
&test('|foo', 'foo');
&test('foo|', 'foo');
&test('a]', 'a]');
&test('a}', 'a}');
&test('a*?x', 'aaax');  # Non-greedy
&test('x{3}', 'axxxxxb');
&test('"(.*)*"', 'The "foo" is "bar"');
&test('"((.)*)*"', 'The "foo" is "bar"');
&test('"((.)*)*"', 'The "foo" is "bar".');
&test('(?=[xf])foo', 'foo');
&test('(bc*d\$|ef*g.|h?i(j|k))', 'bcd');
&test('($)|(x)', '$');
&test('($)|(x)', 'x');
&test('foo\$bar', 'foo$bar');
&test('^a(bc*|b[eh])g|.h$', 'abh');

&test('(a(b)?)*', 'aba');   # Perl doesn't capture $2 but PCRE captures.
&test(".X(.*)*X", "bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
&test("(){0}()", "x");
# &test("(?:(){0}.)+()", "x"); # Panic opt close
&test("((){0}.)+()", "x"); # Panic opt close
&test('^(a()(b)?())+$', "aba"); # $1='a' $2=undef

# CL-PPCRE:629
&test('(A|B)*?CD', 'CD');

# CL-PPCRE:662
&test('((a)*)*', 'a');

&test('(\1)*', 'aa');

sub test
{
    local($pat, $txt) = @_;
    print "$pat $txt\n";

    if ($txt =~ /$pat/)
    {
        print "  \$&=|$&| \$1=|$1| \$2=|$2| \$3=|$3|\n";
        print "  \@-/\@+=|@-|@+|\n";
    }
    else
    {
        print "  not matched\n";
    }
} # test
