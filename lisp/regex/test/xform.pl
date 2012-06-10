while (<>)
{
    if (/^$/)
    {
        print;
        next;
    }

    if (/^\s*#(.*)/s)
    {
        print "; $1";
        next;
    }

    chop;
    local($pat, $limit, $txt, $expect, $comment) = split("\t", $_, 5);

    $pat =~ s/\\/\\\\/g;

    $txt =~ s/\\n/\n/g;
    $txt =~ s/\\t/\t/g;

    $expect =~ s/\\n/\n/g;
    $expect =~ s/\\t/\t/g;

    print "(ext:regex-split \"$pat\" \"$txt\" $limit)\n";
    print "  => (";
    local(@x) = split("[|]", $expect, -1);
    for ($i = 0; $i <= $#x; $i++)
    {
        print " " if 0 != $i;

        if ("nil" eq $x[$i])
        {
            print "nil"
        }
        else
        {
            print "\"$x[$i]\"";
        }
    } # for
    print ")\n";

} # while

