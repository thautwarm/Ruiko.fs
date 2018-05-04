using System;
using System.Collections;
using System.Collections.Generic;

namespace Ruikowa.CSharp
{
    using History = ValueTuple<int, int>;
    public class Trace<T> : IEnumerable<T> where T: class
    {
        public int Count => _trace.Length;
        private _Trace<_Trace<T>> _trace;
        private _Trace<T> Current => _trace[Count - 1];
        public Trace() => _trace = new _Trace<_Trace<T>>();

        public void NewOne()
        {
            
            if (Count < _trace.Mem)
            {
                _trace[_trace.Length++].Length = 0;
            }
            else
            {
                _trace.Add(new _Trace<T>());
            }
        }

        public int MacFetched => _trace.Mem;

        #region consistent with _Trace.
        
        public void Add(T e) => Current.Add(e);

        public T this[int idx] => Current[idx];

        public int Find(T e) => Current.Find(e);

        public int FindSameObj(T e) => Current.FindSameObj(e);

        public T[] GetSlice(int begin, int end) => Current.GetSlice(begin, end);
        public T Pop() => Current.Pop();

        public IEnumerator<T> GetEnumerator() => ((IEnumerable<T>)Current).GetEnumerator();

        
        IEnumerator IEnumerable.GetEnumerator() => ((IEnumerable<T>)Current).GetEnumerator();
        #endregion
        #region version control
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
        #endregion

    }
    internal class _Trace<T> : IEnumerable<T> where T : class
    {
        private List<T> _trace;
        private int _virtualLength;

        public _Trace()
        {
            _trace = new List<T> { };
            _virtualLength = 0;
        }
        public _Trace(T[] arr)
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

        public T[] GetSlice(int begin, int end)
        {
            if (end == -1)
            {
                end = _virtualLength;
            }
            var arr = new T[end - begin];

            for(int i = begin; i < end; ++i)
            {
                arr[i] = _trace[i];
            }
            return arr;
        }


        public int Length
        {
            get => _virtualLength;
            set => _virtualLength = value;
        }

        public int Mem => _trace.Count;
        public int Find(T it)
        {
            for (int i = 0; i < _virtualLength; ++i)
            {
                var e = _trace[i];
                if (e.Equals(it))
                {
                    return i;
                }
            }
            return -1;
        }

        public int FindSameObj(T it)
        {
            for (int i = 0; i < _virtualLength; ++i)
            {
                var e = _trace[i];
                if (e == it)
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

        public T Pop()
        {
            if (--_virtualLength == 0)
            {
                throw new IndexOutOfRangeException($"Pop an empty Trace<{nameof(T)}>.");
            }
            return _trace[Length - 1];
        }

        public IEnumerator<T> GetEnumerator() => ((IEnumerable<T>)_trace).GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() => ((IEnumerable<T>)_trace).GetEnumerator();
    }
}

