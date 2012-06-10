using System.Text.RegularExpressions;
using System;

class RegexTest
{
private static void testIt(string sId, string sPat, string sTxt)
{
    Regex re = new Regex(sPat);

    Console.WriteLine();
    Console.WriteLine("Id:      {0}", sId);
    Console.WriteLine("(test-match \"{0}\" \"{1}\")", sPat, sTxt);

    Match m = re.Match(sTxt);

    if (! m.Success)
    {
        Console.WriteLine("Not matched.");
    }
    else
    {
        Console.Write("(test-case \"{0}\" \"{1}\" \"{2}\"",
            sId,
            sPat,
            sTxt );

        string sSep = " nil '(";
        for (int i = 0; i < m.Groups.Count; i++)
        {
            if (m.Groups[i].Success)
            {
                Console.Write("{0}\"{1}\"", sSep, m.Groups[i].Value);
            }
            else
            {
                Console.Write("{0}nil", sSep, m.Groups[i].Value);
            }

            sSep = " ";
        } // for each group
        Console.WriteLine("))");
    }
} // testIt

public static void Main()
{
    testIt("evita/15", "((a)|(ab))x",   "abx");
    testIt("evita/16", "(a(b)?)*x",     "abax");
    testIt("evita/17", "(a(b)*)*x",     "abax");
    testIt("evita/44", "`(.*)*`",       "The `foo` is `bar`");
    testIt("evita/20", "(\\1)*", "aa");

    testIt("perl-evita/120", "(?<=(A |The |Mr[.]))Foo", "Foo, A Foo, Mr.Foo");

    // What $2 is?
    testIt("perl-584/840", "^([ab]*?)(b)?(c)$", "abac");
        // ("abac" "aba" nil "c")

    // Perl makes $2 unbound.
    testIt("perl-584/860",  "^(a(b)?)+$",       "aba");
    testIt("perl-584/861",  "^(aa(bb)?)+$",     "aabbaa");
    testIt("perl-584/891",  "(.*)(?=c)c",       "abcd");
    testIt("perl-584/897",  "(.*)(?=[bc])c",    "abcd");

    testIt("pcre-2/312",    "(?<!(foo)a)bar",   "foobar");  // "bar" nil
    testIt("pcre-2/725",    "(main(O)?)+",      "mainOmain");
    testIt("pcre-2/730",    "^(a(b)?)+$",       "aba"); // "aba" "a" NIL
    testIt("pcre-2/733",    "^(aa(bb)?)+$",     "aabbaa"); // "aabbaa" "aa" "bb"
    testIt("pcre-2/739",    "^(aa(bb)??)+$",    "aabbaa");
        //  "aabbaa" "aa" "bb"

    testIt("pcre-2/742",    "^(?:aa(bb)?)+$",   "aabbaa");
        // "aabbaa" "bb"

    testIt("pcre-2/751",    "^(?:aa(b(?:b))?)+$",   "aabbaa");
        // "aabbaa" "bb"

    testIt("pcre-2/754",    "^(?:aa(bb(?:b))?)+$",  "aabbbaa");
        // "aabbbaa" NIL

    testIt("pcre-2/757",    "^(?:aa(b(?:bb))?)+$",  "aabbbaa");
        // "aabbbaa" "bbb"

    testIt("pcre-cl/2193", "^(a|){4,5}(?:c|a)c$", "aaaaac");

    // Takes long time. We make subject string shorter than actual case.
    testIt("pcre-1/3824", "(a+)*b", "aaaaaaaaaaaaaaaaaaaa");

    testIt("foo", "\\B*", "foo");
} // Main
} // RegexTest
