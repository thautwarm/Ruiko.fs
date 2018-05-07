using System;
using System.Text.RegularExpressions;
using System.Linq;
using System.Collections.Generic;
using System.IO;
namespace PosSpecificStartsWith
{
    class Program
    {
        static readonly List<Regex> regex =
                new List<string>
                {
                    @"\G\n",
                    @"\Gkeyword1",
                    @"\Gkeyword2",
                    @"\Gkeyword3",
                    @"\G[a-zA-Z_]{1}[a-zA-Z_0-9]*",
                    @"\G\s+",

                }
                .Select(x => new Regex(x))
                .ToList();

        static void Test(string text)
        {
            int pos = 0;
            int n = text.Length;
            while (pos < n)
            {
                foreach (var each in regex)
                {
                    var w = each.Match(text, pos);
                    if (w.Success && w.Index == pos)
                    {
                        pos += w.Value.Length;
                        goto find;
                    }
                }

                pos += 1;
                find:
                continue;

            }
        }
        
        static void Main(string[] args)
        {
            var timer = new System.Diagnostics.Stopwatch();
            var str = File.ReadAllText("text");
            timer.Start();
            for(int i=0; i<100000; i++)
            {
                Test(str);
            }
            Console.WriteLine($"{timer.ElapsedMilliseconds}");
        }
    }
}
