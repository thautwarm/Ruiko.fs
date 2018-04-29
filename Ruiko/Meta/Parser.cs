using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Linq;

namespace Ruiko.Meta
{
    using Result = Result<Ast>;
    public interface IParser
    {
        string Name { get; }
        Result Match(List<Tokenizer> tokens, ParsingTrace parsing, Context context);
        bool Equals(IParser other);
    }


    #region Literal Parsers
    public interface ILiteralParser : IParser
    {
    }
    public class RegexLiteralParser : ILiteralParser
    {
        private readonly Regex s_regex;
        private readonly string s_name;

        public string Name => s_name;
        public RegexLiteralParser(Regex regex)
        {
            s_regex = regex;
            s_name = ConstStringPool.Cast(regex.ToString());
        }
        public Result Match(List<Tokenizer> tokens, ParsingTrace parsing, Context context)
        {
            if (tokens.Count == parsing.Count)
            {
                return Result.Finsihed;
            }

            var head = tokens[parsing.Count];
            if (s_regex.Match(head.Value).Success)
            {
                parsing.NewOne();
                return Result.Matched(new Single(head));
            }
            return Result.Unmatched;
        }
        public bool Equals(IParser other) => other is ILiteralParser && other.Name == Name;
    }

    public class ConstLiteralParser : ILiteralParser
    {
        private readonly string s_name;
        private readonly string s_value;

        public string Name => s_name;

        public ConstLiteralParser(string value)
        {
            s_value = ConstStringPool.Cast(value);
            s_name = ConstStringPool.Cast($"'{value}'");
        }

        Result IParser.Match(List<Tokenizer> tokens, ParsingTrace parsing, Context context)
        {
            if (tokens.Count == parsing.Count)
            {
                return Result.Finsihed;
            }
            var head = tokens[parsing.Count];
            if (s_value == head.Value)
            {
                parsing.NewOne();
                return Result.Matched(new Single(head));
            }
            return Result.Unmatched;
        }

        public bool Equals(IParser other) => other is ILiteralParser && other.Name == Name;
    }

    public class ValueLiteralParser : ILiteralParser
    {

        private readonly string s_value;
        private readonly string s_name;
        public string Name => s_name;

        public ValueLiteralParser(string value)
        {
            s_value = value;
            s_name = ConstStringPool.Cast($"'{value}'");
        }
        Result IParser.Match(List<Tokenizer> tokens, ParsingTrace parsing, Context context)
        {
            if (tokens.Count == parsing.Count)
            {
                return Result.Finsihed;
            }
            var head = tokens[parsing.Count];
            if (s_value.Equals(head.Value))
            {
                parsing.NewOne();
                return Result.Matched(new Single(head));
            }
            return Result.Unmatched;
        }
        public bool Equals(IParser other) => other is ILiteralParser && other.Name == Name;
    }

    public class NameLiteralParser : ILiteralParser
    {
        private readonly string s_name;
        public string Name => s_name;

        public NameLiteralParser(string name)
        {
            s_name = ConstStringPool.Cast(name);
        }

        Result IParser.Match(List<Tokenizer> tokens, ParsingTrace parsing, Context context)
        {
            if (tokens.Count == parsing.Count)
            {
                return Result.Finsihed;
            }
            var head = tokens[parsing.Count];
            if (s_name == head.Name)
            {
                parsing.NewOne();
                return Result.Matched(new Single(head));
            }
            return Result.Unmatched;
        }
        public bool Equals(IParser other) => other is ILiteralParser && other.Name == Name;
    }
    #endregion

    public interface ICombinedParser : IParser
    {
        List<IParser> Structure { get; set; }
    }

    public class AndParser : ICombinedParser
    {
        private string s_name;
        private List<IParser> _structure;
        List<IParser> ICombinedParser.Structure
        {
            get => _structure;
            set => _structure = value;
        }

        public string Name => s_name;

        public AndParser(string name, params IParser[] parsers)
        {
            _structure = new List<IParser>(collection: parsers);
            s_name = name ?? string.Join(" ", parsers.Select(_ => _.Name));
        }


        Result IParser.Match(List<Tokenizer> tokens, ParsingTrace parsing, Context context)
        {
            if (tokens.Count == parsing.Count)
            {
                return Result.Finsihed;
            }

            int where;
            if ((where = parsing.Current.Find(this)) != -1)
            {
                var currentTrace = parsing.Current;
                throw new LeftRecursionDetected(currentTrace, currentTrace.Length - 1, where);
            }

            var history = parsing.Commit();
            var parsed = new Nested(s_name);

            Result result;
            foreach (var parser in _structure)
            {

                try
                {
                    result = parser.Match(tokens, parsing, context);
                }
                catch (LeftRecursionDetected e)
                {
                    if (e.Depth == 0)
                        goto LR;
                    --e.Depth;
                    throw e;
                }

                if (result.Status == Status.Matched)
                {
                    parsed.Structure.Add(result.Value);
                    continue;
                }

                // unmatched here:
                parsing.Reset(history);
                return result;
            }

            return Result.Matched(parsed);

            LR:
            throw new NotImplementedException("LR handling.");
        }

        public bool Equals(IParser other) => other is ICombinedParser && other.Name == Name;
    }

    public class OrParser : ICombinedParser
    {

        private string s_name;
        private List<IParser> _structure;
        List<IParser> ICombinedParser.Structure
        {
            get => _structure;
            set => _structure = value;
        }

        public string Name => s_name;

        public OrParser(string name, params IParser[] parsers)
        {
            _structure = new List<IParser>(collection: parsers);
            s_name = name ?? string.Join(" | ", parsers.Select(_ => _.Name));
        }

        Result<Ast> IParser.Match(List<Tokenizer> tokens, ParsingTrace parsing, Context context)
        {
            if (tokens.Count == parsing.Count)
            {
                return Result.Finsihed;
            }

            int where;
            if ((where = parsing.Current.Find(this)) != -1)
            {
                var currentTrace = parsing.Current;
                throw new LeftRecursionDetected(currentTrace, currentTrace.Length - 1, where);
            }

            var history = parsing.Commit();


            Result result;
            foreach (var parser in _structure)
            {

                try
                {
                    result = parser.Match(tokens, parsing, context);
                }
                catch (LeftRecursionDetected e)
                {
                    if (e.Depth == 0)
                        goto LR;
                    --e.Depth;
                    throw e;
                }

                if (result.Status == Status.Matched)
                {
                    return result;
                }

                // unmatched here:
                parsing.Reset(history);
                continue;
            }

            return Result.Unmatched;

            LR:
            throw new NotImplementedException("LR handling.");

        }

        public bool Equals(IParser other) => other is ICombinedParser && other.Name == Name;


    }

}
