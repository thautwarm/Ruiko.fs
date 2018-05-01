using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Linq;

namespace Ruiko.Meta
{
    using Result    = Result<IAST>;
    using Predicate = Func<IAST, Status>;
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

        Status ThrowRule(IAST ast);
    }

    public class AndParser : ICombinedParser
    {
        private string s_name;
        private List<IParser> s_structure;
        private Predicate s_throwRule;

        public Status ThrowRule(IAST ast) => s_throwRule(ast);

        List<IParser> ICombinedParser.Structure
        {
            get => s_structure;
            set => s_structure = value;
        }

        public string Name => s_name;

        string IParser.Name => throw new NotImplementedException();

        public AndParser(string name, IParser[] parsers, Predicate throwRule)
        {
            s_throwRule = throwRule;
            s_structure = new List<IParser>(collection: parsers);
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

            foreach (var parser in s_structure)
            {

                try
                {
                    result = parser.Match(tokens, parsing, context);
                }
                catch (LeftRecursionDetected e)
                {
                    /// Handling Left Recursions.
                    if (e.Depth == 0)
                        goto LR;
                    --e.Depth;
                    throw e;
                }

                if (result.Status != Status.Matched)
                {
                    parsing.Reset(history);
                    return result;
                }


                if (parser is ISequenceParser)
                {
                    var nested = result.Value as Nested;
                    if (s_throwRule == null)
                    {
                        parsed.Structure.AddRange(nested.Structure);
                    }
                    else
                    {
                        foreach(var each in nested.Structure)
                        {
                            var status = ThrowRule(each);
                            switch (status)
                            {
                                case Status.Matched:
                                    parsed.Structure.Add(each);
                                    continue;
                                case Status.Ignore:
                                    continue;
                                case Status.Return:
                                    break;
                                case Status.Finished:
                                    return Result.Finsihed;
                                case Status.Unmatched:
                                    goto Unmatched;
                                case Status.Exit:
                                    return Result.Exit;
                            }
                        }
                    }
                }
                else
                {
                    if (s_throwRule == null)
                        parsed.Structure.Add(result.Value);
                    else
                    {
                        switch (ThrowRule(result.Value))
                        {
                            case Status.Matched:
                                parsed.Structure.Add(result.Value);
                                break;
                            case Status.Ignore:
                                break;
                            case Status.Return:
                                return Result.Matched(parsed);
                            case Status.Finished:
                                return Result.Finsihed;
                            case Status.Unmatched:
                                goto Unmatched;
                            case Status.Exit:
                                return Result.Exit;
                        }
                    }
                }
            }
            return Result.Matched(parsed);


            Unmatched:
            parsing.Reset(history);
            return Result.Unmatched;

            LR:
            throw new NotImplementedException("LR handling.");
        }

        public bool Equals(IParser other) => other is ICombinedParser && other.Name == Name;



    }

    public class OrParser : ICombinedParser
    {

        private string s_name;
        private List<IParser> s_structure;
        private Predicate<IAST> s_throwRule;
        Status ICombinedParser.ThrowRule(IAST ast) => s_throwRule(ast);

        public string Name => s_name;

        List<IParser> ICombinedParser.Structure
        {
            get => s_structure;
            set => s_structure = value;
        }

        public OrParser(string name, IParser[] parsers, Predicate<IAST> throwRule)
        {
            s_throwRule = throwRule;
            s_structure = new List<IParser>(collection: parsers);
            s_name = name ?? string.Join(" | ", parsers.Select(_ => _.Name));
        }

        Result<IAST> IParser.Match(List<Tokenizer> tokens, ParsingTrace parsing, Context context)
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
            foreach (var parser in s_structure)
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

    public interface ISequenceParser : IParser
    {

    }

}
