using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;
namespace Ruiko.Meta
{
    using History = ValueTuple<int, int>;
    public class ParsingTrace
    {
        public int Count => _trace.Length;
        private Trace<Trace<IParser>> _trace;
        public Trace<Trace<IParser>> Trace => _trace;

        public Trace<IParser> Current => _trace[Count - 1];
        public ParsingTrace()
        {
             _trace = new Trace<Trace<IParser>>();
        }

        public void AppendCurrentTrace(IParser parser)
        {
            _trace[Count - 1].Add(parser);
        }
        

        public void NewOne()
        {
            if (Count < _trace.Mem)
            {
                _trace[_trace.Length++].Length = 0;
            }
            else
            {
                _trace.Add(new Trace<IParser>());
            }
        }

        public History Commit()
        {
            return (Count, _trace[Count].Length);
        }

        public void Reset(History history)
        {
            var (count, length) = history;
            _trace.Length = count;
            _trace[count - 1].Length = length;
        }

        public int MacFetched => _trace.Mem;
    }

    public class Context
    {

    }
}
