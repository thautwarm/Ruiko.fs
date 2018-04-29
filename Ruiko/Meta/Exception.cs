using System;
using System.Collections.Generic;
using System.Text;

namespace Ruiko.Meta
{
    public class LeftRecursionDetected: Exception
    {
        public Trace<IParser> Parsing;
        public int Begin;
        public int End;
        public int Depth;
        public LeftRecursionDetected(Trace<IParser> parsing, int begin, int end) : base()
        {
            Parsing = parsing;
            Begin   = begin;
            End     = end;
            Depth   = begin - end;
        }

    }
}
