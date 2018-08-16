using System;
using System.Collections;
using System.Collections.Generic;

namespace Ruikowa.CSharp
{
    public class Trace<T>
    {
        private List<T> records;
        private int virtual_len;
        public Trace()
        {
            records = new List<T> { };
            virtual_len = 0;
        }
        public void Clear()
        {
            virtual_len = 0;
        }

        public void Reset(int history)
        {
            virtual_len = history;
        }

        public void Append(T e)
        {
            var s_len = virtual_len;
            if (MaxFetched == s_len)
            {
                records.Add(e);
                virtual_len++;
                return;
            }
            virtual_len++;
            records[s_len] = e;
        }

        public bool Inc(Func<T> Factory)
        {
            var s_len = virtual_len;
            if (MaxFetched == s_len)
            {
                records.Add(Factory());
                virtual_len++;
                return true;
            }
            virtual_len++;
            return false;
        }

        public int Commit() => virtual_len;

        public int EndIndex => virtual_len - 1;
        public int MaxFetched => records.Count;

        public int Size()
        {
            return virtual_len;
        }

        public T this[int i]
        {
            get
            {
                if (i >= virtual_len)
                {
                    throw new IndexOutOfRangeException();
                }
                return records[i];
            }
        }

        public bool Contains(T e)
        {
            for(int i = 0; i < virtual_len; ++i)
            {
                if (records[i].Equals(e))
                {
                    return true;
                }
            }
            return false;
        }
    }
  
}

