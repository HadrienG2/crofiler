Ways to render lists :

```
<HEADER> A <SEP> B <SEP> C <TRAILER>
```

```
<HEADER>
    A <SEP>
    B <SEP>
    C
<TRAILER>
```

```
<HEADER> A <SEP>
         B <SEP>
         C <TRAILER>
```

```
<HEADER> A <SEP> B
           <SEP> C <TRAILER>
```

```
<HEADER>
    A
    <SEP> B
    <SEP> C
<TRAILER>
```

---

What can be displayed:

- Paths
    * Decomposed as list of textual components
    * Put separator inbetween components except first component if Unix root
    * Use shortened file name when full filename cannot be displayed
- Identifiers
- `Option<T>`
    * Display T if present, nothing otherwise (may want to reconsider)
- Template parameters
    * Normal rule = list with '<' header, ',' separator, '>' trailer (see above
      for possible ways to display lists)
    * Special rule = "<, void>"
    * List items are types or values
