# crofiler: Easier C++ build profiling

Understanding why C++ builds get slow has become a lot easier since clang
introduced their -ftime-trace build tracing feature.

However, it is not easy enough yet because currently available -ftime-trace
visualizations (Chrome's about:tracing and to a lesser extent Speedscope) remain
too focused on the raw temporal sequence of activities that were undertaken by
clang, and not enough on actual build profiling questions such as which source
tres, C++ namespaces, individual class and function templates... account for
the highest share of expended compilation time.

`crofiler`, the **C**lang **C**++ **C**ompilation p**rofiler**, aims to provide
more immediate answers to such questions.
