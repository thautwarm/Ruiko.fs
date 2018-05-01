using System;
using System.Collections.Generic;
using System.Text;

namespace Ruiko.Meta
{
    public interface IAST
    {
       string Name { get; }
    }

    public class Single: IAST
    {
        public readonly Tokenizer Token;
        public string Name => Token.Name;

        public Single(Tokenizer token)
        {
            Token = token;
        }
    }

    public class Nested: IAST
    {
        public List<IAST> Structure;
        private readonly string s_name;
        public string Name => s_name;

        public Nested(string name)
        {
            s_name = name;
        }

        public Nested(string name, IAST[] structure)
        {
            s_name = name;
            Structure = new List<IAST>(structure);
        }
    }

   
}
