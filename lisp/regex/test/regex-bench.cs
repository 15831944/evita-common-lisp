using System.Text.RegularExpressions;
using System;

// From Mastering Regular Expressions, Jeffry E. F. Friedl, O'Reily, 2002
// P.237

// CPU: i686.8.6-863MHz
// .NET: 1.1.4322
//  Alt  7.891 sec.
//  Cc   0.941 sec.

class RegexBench
{
    private static string s_sTestString = "";
    private const int nTimesToDo = 1000;

    private static void timeIt(string sRe)
    {
        Regex oRegex = new Regex(sRe);

        Console.WriteLine("Start {0}", oRegex.ToString());

        DateTime dteStart = DateTime.Now;
        for (int i = 0; i < nTimesToDo; i++)
        {
            oRegex.Match(s_sTestString);
        } // for
        DateTime dteEnd = DateTime.Now;
        TimeSpan oSeconds = dteEnd.Subtract(dteStart);

        Console.WriteLine(
            "It takes {0} seconds.",
            (oSeconds.Ticks / TimeSpan.TicksPerMillisecond) / 1000.0 );
    } // timeIt

    public static void Main()
    {
        for (int i = 0; i < 1000; i++)
        {
            s_sTestString += "abababdedfg";
        } // for

        timeIt("(?<name1>foo)(?<name1>bar)");
        timeIt("^(a|b|c|d|e|f|g)+$");
        timeIt("^[a-g]+$");
    } // Main
} // RegexBench
