var EvRegEx = new ActiveXObject("Evita.RegEx");
EvRegEx.Compile("(?:.*)*");
"foo".match(/(?:.*)*/);

test(".X(.+)+X", "bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");

function test(pat, text)
{
    test_ms(pat, text);
    test_ev(pat, text);
} // test


function test_ev(pat, text)
{
    WSH.Echo("EV:", pat);
    if (EvRegEx.Match(pat, text))
    {
        WSH.Echo("matched");
    }
    else
    {
        WSH.Echo("not matched");
    }
} // test_ev


function test_ms(pat, text)
{
    WSH.Echo("MS:", pat);
    if (text.match(pat))
    {
        WSH.Echo("matched");
    }
    else
    {
        WSH.Echo("not matched");
    }
} // test_ev
