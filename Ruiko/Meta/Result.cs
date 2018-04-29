using System;
using System.Collections.Generic;
using System.Text;

namespace Ruiko.Meta
{
    public enum Status
    {
        Unmatched,
        Matched,
        Finished
    }
    public class Result<T> where T : class
    {
        


        public readonly Status Status;
        public T Value;
        public Result(Status status, T value)
        {
            Status = status;
            Value  = value;
        }

        public static Result<T> Matched(T value = null)   => new Result<T>(Status.Matched, value);
        public static Result<T> Unmatched       = new Result<T>(Status.Unmatched, null);
        public static Result<T> Finsihed        = new Result<T>(Status.Finished, null);

    }
}
