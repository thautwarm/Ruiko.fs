using System;
using System.Collections.Generic;
using System.Text;

namespace Ruiko.Meta
{
    public static class ConstStringPool
    {
        public static Dictionary<string, string> Pool;

        public static string Cast(string value)
        {
            var res = Pool.GetValueOrDefault(value);
            if (res == default(string))
            {
                Pool[value] = value;
                res = value;
            }
            return res;
        }
    }
   

    public class Tokenizer
    {
        public readonly int Lineno;
        public readonly int Colno;

        public readonly string Value;
        public readonly string Name;

        public Tokenizer(string name, string value, int lineno, int colno)
        {
            Name   = ConstStringPool.Cast(name);
            Value  = value;
            Lineno = lineno;
            Colno  = colno;
        }
    }
}
