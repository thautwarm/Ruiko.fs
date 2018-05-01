using System;
using System.Collections.Generic;
using System.Text;

namespace Ruiko
{
    static class Ext
    {
        public static TR By<TI, TR>(this TI subject, Func<TI, TR> by)
            => by(subject);

        public static void By<TI>(this TI subject, Action<TI> by) 
            => by(subject);


    }
}
