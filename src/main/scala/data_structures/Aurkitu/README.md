```
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

   @@@@@@@@@@@@@  *@@@@@%  @@@@@@  @@@@@@@@@@      @@@@@@  %@@@@@@  #@@@@@  @@@@@@@@@@@@@ @@@@@@   @@@@@@
   @@.&@@@@@ @@@   .@@@@    @@@@    @@@@  #@@@@     @@@@   @@@@     /@@@@@  @@. @@@@  @@@  @@@@    @@@@@
      @@@@@@%      .@@@@    @@@@    @@@@   @@@@.    @@@@/@@@@       @@@@@@,     @@@@       @@@@    @@@@@
     @@@  @@@*     .@@@@    @@@@    @@@@@@@@@&      @@@@@@@@*        @@@@@      @@@@       @@@@    @@@@@
   .@@@@@@@@@@@     @@@@   *@@@@    @@@@ @@@ @@@@@  @@@@  @@@@@      @@@@@      @@@@       @@@@@   @@@@&
  @@@@@ @@ @@@@@     @@@@@@@@@@    @@@@@  *@@@@@@( @@@@@.   @@@@@.   @@@@@      @@@@@       #@@@@@@@@@

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
```

Aurkitu (the [Euskara](https://en.wikipedia.org/wiki/Basque_language) word for "to find") is a data
structure that was born from a simple question: for a given list of words we know how to find all
the words that start with a given prefix quickly, using a Trie. But what about finding all the words
that contain a given substring? Surely we can find a data structure that provides better runtime
complexity than `Θ(n)`.

## How does it work?

Upon ingesting a word, an Aurkitu will first generate a unique ID for this word, and store it in a
map of (id -> word) shape.
It will also slice up and the word into tuples representing each character and its position in the
word. Thus, `baz` gets turned into `[('b', 0), ('a', 1), ('z', 2)]`. It will then use those tuples
to populated two more maps:

- the `characterPositions` map, which stores all the positions any character appears in, regarless
of the word.

- the `index` map, which stores all the (char, positionInWord) tuples of all the words ingested, and
the IDs of the words in which the appear.

Let's say we ingest the following words: "foo", "bar", "baz". Here's how the resulting Aurkitu will
look like:

```
Aukritu(
  words=Map(
    (0 -> "foob"),
    (1 -> "bar"),
    (2 -> "baaz"),
  ),
  characterPositions=Map(
    (f -> {0}),
    (o -> {1, 2}),
    (b -> {0, 3}),
    (a -> {1, 2}),
    (r -> {2}),
    (z -> {3}),
  ),
  index=Map(
    (f, 0) -> {0},
    (o, 1) -> {0},
    (o, 2) -> {0},
    (b, 3) -> {0},
    (b, 0) -> {1, 2},
    (a, 1) -> {1, 2},
    (r, 2) -> {1},
    (a, 2) -> {2},
    (z, 3) -> {2},
  ),
)
```

From there, say we want to look for words containing the string "ba".
The first thing we'll do is to look up all the characters in the `characterPositions` map: `b`
returns `{0, 3}` and `a` returns `{1, 2, 4}}`. This means that across all the stored words, the
following tuples exist: `[(b, 0), (b, 3)]` and `[(a, 1), (a, 2), (a, 4)]`.

From there, what we'll take each of the tuples we found for the letter `b` in the `index`.
We get `{1, 2}` and `{0}`. Then, we look up `(a, 1)` and `(a, 4)`. Why? Because we took the indexes
we found for `b` and incremented them, because `a` has to come right after `b` in the words we're
trying to find. For `(a, 1)` and `(a, 4)`, we get `{1, 2}` and `{}`.
Then what we do is to intersect the results for the first letter with the corresponding results for
the second letter: `{1, 2} ⋂ {1, 2}` equals `{1, 2}` and `{0} ⋂  {}` equals `{}`. So, we know we
have two matches: the IDs 1 and 2. We look them up in the `words` map and get our matching words:
"bar" and "baaz".

We have described this process with a two-letters pattern, but as you can imagine it can be
performed with as many letters as needed, we just keep computing intersections until we reach the
end of the pattern.
