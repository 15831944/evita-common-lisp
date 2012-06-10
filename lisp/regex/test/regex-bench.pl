# use Time::HiRes 'time';

# From Mastering Regular Expressions, Jeffry E. F. Friedl, O'Reily, 2002
# P.237


$TIMES_TO_DO = 1000;

$text = "abababdedfg" x 1000;

&time_it(qr/^(a|b|c|d|e|f|g)+$/, $text);    # 7 sec.
&time_it(qr/^[a-g]+$/, $text);

sub time_it
{
    local($pat, $text) = @_;
    print "Start $pat\n";

    $start = time();

    for ($i = 0; $i < $TIMES_TO_DO; $i++)
    {
        $text =~ $pat;
    }

    $end = time();

    printf(" It takes %.3f sec.", $end - $start);
} # time_it
