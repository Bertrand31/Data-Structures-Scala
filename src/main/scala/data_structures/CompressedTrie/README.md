## Implementing a Trie

In order to implement a trie, one has a few ways to go about it.
First of all, we have to determine whether we want to support words containing symbols, or letters
outside the basic English alphabet (26 characters, no accents).

If we chose to stick with the basic 26 characters, then each node of the Trie can contain a
26-elements long array, where each index represents a character and contains a sub-Trie.

In that case, we could even picture an optimization inspired by Hash Array Mapped Tries, and
compress the structure by storing a simple integer along with the array, and  use it as a 32-bits
bitset. Then, the array next to it would no longer be 26 items long, but would only contain
sub-Tries for characters that are actually stored, and we'd use the bitset to find out what index
the elements in the array correspond to.
[Read more on that technique](https://en.wikipedia.org/wiki/Hash_array_mapped_trie#Operation).

This looks pretty good, however this only fits a rather narrow use-case since we can only store
english words. If our use-case includes storing letters with accents, symbols, or any alphabet other
than the English one, we have to look for other solutions.

An instinctive solution would be to take the intial implementation using 26-elements long arrays,
and replace these with HashMaps, in which the keys would be characters, and values would be
sub-Tries. However, this would blow up memory usage. In Scala, immutable HashMaps are implemented
using HAMTs and thus, will always take more space that simpler data structures on small datasets.

Instead, let's try to improve on our previous solutions. Naturally, we can't adapt the first
approach to support to full 16 bits a character entails: that would mean that each node would hold
a 65536-elemnts long array, which would be extremely wasteful.

We also can no longer use a single Int to serve as a bitset, because an Int in Scala is only 32
bits long. A Long would not work either by the way, since 64 is still far from 65535.

We're on the right track though: instead of using a single number as a bitset, let's now use a
full-blown bitset. Like [this one for example](../BitSet.scala).

We can now store any character, while maintaining excellent runtime performance and a good memory
footprint. However, if we look at the way our bitset is going to be used, we can notice that the
indices it's going to hold will likely be both sparse and close to eacher other (imagine we're
storing Korean and English words for example, we'll have indices around the fourty thousands and
around one hundred).

Because of this, and because of the way traditional bitsets work, we will end up wasting some
space for unoccupied ranges of bits in-between those clusters of characters.

## Introducing the Compressed Trie

Building on what we have at this point, we can introduce a slightly different variation addressing
our concerns about our previous approach.

We can replace our traditional bitset with a compressed bitset, and thus trade a little bit of
runtime speed for a lot of space for most datasets. This gives us our Compressed Trie.

For our particular implementation, [RoaringBitmaps](https://github.com/RoaringBitmap/RoaringBitmap)
are used. They are used by Apache Spark, Apache Hive, Apache Druid, Netflix Atlas, etc. However, it
being a Java library, I have [extended it](./RoaringBitmapImproved.scala) using an _Implicit Class_
to give it a more immutable look (even though it remains a mutable data structure underneath) and
add a few methods we need.

## Performance comparison

For demonstration purposes, I have compared the construction and the search through both a naive
implementation of a Trie using arrays of `Option`s, and our Compressed Trie:

```
============================
Profiling basicTrie.apply:
Median          45 622 ns
============================
Profiling compressedTrie.apply:
Median          22 631 ns
============================
Profiling basicTrie.keysWithPrefix:
Median          21 062 ns
============================
Profiling compressedTrie.getNBelow:
Median          2 288 ns
```

## Alternative

It is interesting to note that for our purpose, a Radix Tree would also work, and would provide a
more compact representation of our dataset, given how small and sparse it is. Although it will
provide identical worst-case time complexities, it may also provide better runtime performance for
search operations.
