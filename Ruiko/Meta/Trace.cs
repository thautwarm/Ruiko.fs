using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace Ruiko.Meta
{
    using History = ValueTuple<int, int>;
    public class Trace<T> : IEnumerable<T> where T : class
    {
        private List<T> _trace;
        private int _virtualLength;

        public Trace()
        {
            _trace = new List<T> { };
            _virtualLength = 0;
        }
        public Trace(T[] arr)
        {
            _trace = new List<T>(arr);
            _virtualLength = 0;
        }
        public T this[int idx]
        {
            get
            {
                return _trace[idx];
            }
            set
            {
                _trace[idx] = value;
            }
        }

        public int Length
        {
            get => _virtualLength;
            set => _virtualLength = value;
        }

        public int Mem => _trace.Count;
        public int Find(T each)
        {
            for (int i = 0; i < _virtualLength; ++i)
            {
                var e = _trace[i];
                if (e.Equals(each))
                {
                    return i;
                }
            }
            return -1;
        }

        public void Add(T e)
        {
            if (Mem <= _virtualLength)
            {
                _trace.Add(e);
            }
            else
            {
                _trace[_virtualLength] = e;
            }
            ++_virtualLength;
        }

        public void Pop()
        {
            if (--_virtualLength == 0)
            {
                throw new IndexOutOfRangeException($"Pop an empty Trace<{nameof(T)}>.");
            }
        }


        public IEnumerator<T> GetEnumerator() => ((IEnumerable<T>)_trace).GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() => ((IEnumerable<T>)_trace).GetEnumerator();
    }
}

