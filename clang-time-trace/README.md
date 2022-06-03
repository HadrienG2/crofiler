# A parser for the output of clang's `-ftime-trace`

This is a parser for the JSON output of clang's
[`-ftime-trace`](https://aras-p.info/blog/2019/01/16/time-trace-timeline-flame-chart-profiler-for-Clang/).

It aims to provide not just structured access to the inner data (which your
favorite generic JSON library will handle perfectly well), but to analyze it
down to its basic syntax atoms into a strongly typed and hierarchical form,
easing the existence of higher-level visualization tools like
[crofiler](https://github.com/HadrienG2/crofiler).
