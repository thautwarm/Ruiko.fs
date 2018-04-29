using System;
using System.Collections.Generic;
using System.Text;

namespace Ruiko.Meta
{
    public interface Ast
    {
       string Name { get; }
    }

    public class Single: Ast
    {
        public readonly Tokenizer Token;
        public string Name => Token.Name;

        public Single(Tokenizer token)
        {
            Token = token;
        }
    }

    public class Nested: Ast
    {
        public List<Ast> Structure;
        private readonly string s_name;
        public string Name => s_name;

        public Nested(string name)
        {
            s_name = name;
        }

        public Nested(string name, Ast[] structure)
        {
            s_name = name;
            Structure = new List<Ast>(structure);
        }
    }

   
}
