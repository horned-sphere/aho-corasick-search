package simplesearch.algorithm

import org.scalatest.{FunSpec, Matchers}
import simplesearch.structures.Trie
import simplesearch.structures.Trie.{TrieNode, TrieRoot}

/**
  * Unit tests of [[AhoCorasick]].
  */
class AhoCorasickTest extends FunSpec with Matchers {

  import AhoCorasickTest._

  describe("The computation of dictionary suffixes") {

    it("should compute the expected back references") {

      val trie = Trie[String](Seq("a", "ab", "bab", "bc", "bca", "c", "caa"), identity)

      val maxSuffixes = AhoCorasick.computeMaximalSuffixes(trie)
      val dictSuffixes = AhoCorasick.computeDictionarySuffixes(trie, maxSuffixes)

      dictSuffixes.size shouldEqual 10

      dictSuffixes(get(trie, 'a')) shouldEqual trie
      dictSuffixes(get(trie, 'b')) shouldEqual trie
      dictSuffixes(get(trie, 'c')) shouldEqual trie
      dictSuffixes(get(trie, 'a', 'b')) shouldEqual trie
      dictSuffixes(get(trie, 'b', 'a')) shouldEqual get(trie, 'a')
      dictSuffixes(get(trie, 'b', 'c')) shouldEqual get(trie, 'c')
      dictSuffixes(get(trie, 'c', 'a')) shouldEqual get(trie, 'a')
      dictSuffixes(get(trie, 'b', 'a', 'b')) shouldEqual get(trie, 'a', 'b')
      dictSuffixes(get(trie, 'b', 'c', 'a')) shouldEqual get(trie, 'a')
      dictSuffixes(get(trie, 'c', 'a', 'a')) shouldEqual get(trie, 'a')
    }

  }

  describe("The detection of phrases in a sentence") {

    it("should produce the expected detections") {

      val ac1 = AhoCorasick(Seq("a", "ab", "bab", "bc", "bca", "c", "caa"), identity[String])

      ac1.findPhrasesIn("abccab") shouldEqual List("a", "ab", "bc", "c", "c", "a", "ab")

      val ac2 = AhoCorasick(Seq("he", "she", "his", "hers"), identity[String])

      ac2.findPhrasesIn("he hers shehis") shouldEqual List("he", "he", "hers", "she", "he", "his")

      val ac3 = AhoCorasick(Seq(" indian ", " thai ", " sushi "), identity[String])

      ac3.findPhrasesIn(" i would like some thai food ") shouldEqual List(" thai ")

      val ac4 = AhoCorasick(Seq(" indian ", " thai ", " sushi "), identity[String])

      ac4.findPhrasesIn(" i would like some vietnamese food ") shouldEqual List.empty

      val ac5 = AhoCorasick(Seq(" indian ", " thai ", " thai food ", " sushi "), identity[String])

      ac5.findPhrasesIn(" i would like some thai food ") shouldEqual List(" thai ", " thai food ")

    }

  }

}

object AhoCorasickTest {

  /**
    * Get the node of a trie by following a sequence of letters.
    * @param trie The trie.
    * @param path The path to follow.
    * @return The node that was found.
    */
  def get(trie : TrieRoot[String], path : Char*) : TrieNode[String] = {
    val root : TrieNode[String] = trie
    path.foldLeft(root) { case (acc, c) => acc.children(c)}
  }

}
