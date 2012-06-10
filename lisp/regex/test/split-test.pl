for ($limit = -1; $limit < 20; $limit++)
{
    &show(",|(-)", "1-10,20,,,", $limit);
}

&show("(a)|(b)|(c)", "", -1);
&show(".?", "", -1);
&show(".?", "", 0);
&show(".x", "xxx", 0);


&show("x", "", -1);
&show("x", "",  0);
&show("x", "",  1);
&show("x", "",  2);

&show(qr'^a$'m, "a b a\na d a", 20);

sub show
{
    local($pat, $txt, $limit) = @_;
    local(@x) = split($pat, $txt, $limit);

    $txt =~ s/\n/\\n/g;

    print "$pat\t$limit\t$txt\t";

    for ($i = 0; $i <= $#x; $i++)
    {
        print "|" if 0 != $i;

        if (! defined($x[$i]))
        {
            print "nil";
        }
        else
        {
            $x[$i] =~ s/\n/\\n/g;
            print $x[$i];
        }
    } # for

    print "\t; " . ($#x + 1) . " elts.\n";
} # show
