# A basic parser for C++ entity names

This is a C++ parser whose design goal is to understand enough of the C++
grammar, extended with a couple of common compiler display extensions (like
lambda locations) to produce a valid AST of all C++ entity names in the output
of clang's [`-ftime-trace`](https://aras-p.info/blog/2019/01/16/time-trace-timeline-flame-chart-profiler-for-Clang/).

Producing a 100% semantically correct parse is not a goal, as this is impossible
without extra source code context (without that, you cannot even tell if an
identifier refers to a type or a value, which affects the meaning of basic
syntax tokens like `<`). However, the parse should neither violate the language
grammar nor assume the use of obscure language features where common ones would
suffice. For example, parsing parsing multiple function parameters as a single
value expression involving the comma operator `,` is considered a bug.

If you find an input for which this parser refuses to produce an AST or produces
one that you find obviously bogus, please open an issue featuring the input and
the reason why you think the output is wrong!