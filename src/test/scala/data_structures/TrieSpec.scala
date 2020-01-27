import data_structures.Trie

import org.scalatest.flatspec.AnyFlatSpec

class TrieSpec extends AnyFlatSpec {

  behavior of "The Trie implementation"

  val newTrie = Trie()

  behavior of "the apply method"

  it should "create an empty Trie" in {
    assert(newTrie.isEmpty)
  }

  behavior of "the + method"

  val trieWithBar = newTrie + "bar"

  it should "add an element to the trie" in {
    assert(!trieWithBar.isEmpty)
  }

  it should "not mutate the original trie" in {
    assert(newTrie.isEmpty) // No mutation
  }

  behavior of "the contains method"

  it should "find the existing 'bar' string" in {
    assert(trieWithBar.contains("bar"))
  }

  it should "return false for a non-existing string" in {
    assert(!trieWithBar.contains("baz"))
  }

  val trieWithFooBar = trieWithBar + "foo"
  val complexTrie = trieWithFooBar + "barreau"

  behavior of "the keys method"

  it should "return all the words that were added to the trie, in no particular order" in {
    assert(trieWithFooBar.keys == List("bar", "foo"))
    assert(complexTrie.keys == List("bar", "barreau", "foo"))
  }

  behavior of "the keysWithPrefix method"

  it should "return all stored words starting with a given prefix" in {
    assert(complexTrie.keysWithPrefix("barr") == List("barreau"))
    assert(complexTrie.keysWithPrefix("bar") == List("bar", "barreau"))
    assert(complexTrie.keysWithPrefix("baz") == List())
  }

  behavior of "the ++ (merging) method"

  it should "merge two tries' contents into a new trie" in {
    val sampleTrie = Trie("foo", "bar")
    val sampleTrie2 = Trie("baz")
    val mergedTrie = sampleTrie ++ sampleTrie2
    assert(mergedTrie.keys.sorted == List("bar", "baz", "foo"))
  }
}
