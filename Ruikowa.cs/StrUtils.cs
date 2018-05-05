using System.Text.RegularExpressions;
using System;

namespace Ruikowa.CSharp
{
    public static class StrUtils
    {
        public static bool StartsWithAt(string subject, string @object, int pos)
        {
            if (subject.Length - pos < @object.Length)
            {
                return false;

            }
            
            int n = @object.Length;
            for(int i = 0; i < n; ++i) {
                if (!subject[pos + i].Equals(@object[i]))
                    return false;
            }
            return true;
        }
    }
}
