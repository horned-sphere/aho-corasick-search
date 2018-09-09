package simplesearch.structures

import org.scalatest.{FunSpec, Matchers}
import simplesearch.structures.Trie.{TrieBranch, TrieRoot}

import scala.collection.immutable.HashMap

/**
  * Unit tests for [[Trie]].
  */
class TrieTest extends FunSpec with Matchers {

  import TrieTest._

  describe("Construction of a trie") {

    it("should produce a linear tree for a single word") {

      val trie = Trie[String](Seq("the"), identity)

      trie shouldEqual root('t' -> branch('t', 'h' -> branch('h', 'e' -> wordBranch('e', "the"))))
    }

    it("should create a tree with three branches from the root for different letters") {
      val trie = Trie[String](Seq("a", "b", "c"), identity)

      trie shouldEqual root('a' ->  wordBranch('a', "a"),
        'b' ->  wordBranch('b', "b"),
        'c' ->  wordBranch('c', "c"))
    }

    it("should have appropriate branch points for more complex word lists") {
      val trie = Trie[String](Seq("he", "she", "his", "hers"), identity)

      val sheBranch = branch('s', 'h'-> branch('h', 'e' -> wordBranch('e', "she")))

      val isBranch = branch('i', 's' -> wordBranch('s', "his"))

      val ersBranch = wordBranch('e', "he", 'r' -> branch('r', 's' -> wordBranch('s', "hers")))

      val hBranch = branch('h', 'i' -> isBranch, 'e' -> ersBranch)

      val expected = root('h' -> hBranch, 's' -> sheBranch)

      trie shouldEqual expected
    }

  }

  describe("Enumerating a trie in by breadth first search") {

    it("should visit parents before children") {
      val trie = Trie[String](Seq("he", "she", "his", "hers"), identity)

      val bfs = Trie.bfsStream(trie).toList

      val level1 = trie.children.values.map(branch => (trie, branch)).toSet

      val level2 = level1.flatMap {
        case (_, parent @ TrieBranch(_, _, children)) => children.values.map(child => (parent, child))
      }

      val level3 = level2.flatMap {
        case (_, parent @ TrieBranch(_, _, children)) => children.values.map(child => (parent, child))
      }

      val level4 = level3.flatMap {
        case (_, parent @ TrieBranch(_, _, children)) => children.values.map(child => (parent, child))
      }

      bfs.length shouldEqual level1.size + level2.size + level3.size + level4.size

      val (actual1, remainder1) = bfs.splitAt(level1.size)

      actual1.toSet shouldEqual level1

      val (actual2, remainder2) = remainder1.splitAt(level2.size)

      actual2.toSet shouldEqual level2

      val (actual3, actual4) = remainder2.splitAt(level3.size)

      actual3.toSet shouldEqual level3

      actual4.toSet shouldEqual level4
    }

  }

}

object TrieTest {

  /**
    * Create a trie root.
    * @param children The children of the root.
    * @return The root.
    */
  def root(children : (Char, TrieBranch[String])*) : TrieRoot[String] = TrieRoot(HashMap(children: _*))

  /**
    * Create a trie branch that does not correspond to a dictionary entry.
    * @param c The character at the node.
    * @param children The children of the node.
    * @return The node.
    */
  def branch(c : Char, children : (Char, TrieBranch[String])*): TrieBranch[String] =
    TrieBranch[String](c, None, HashMap(children: _*))

  /**
    * Create a trie branch that does correspond to a dictionary entry.
    * @param c The character at the node.
    * @param word The word.
    * @param children The children of the node.
    * @return The node.
    */
  def wordBranch(c : Char, word : String, children : (Char, TrieBranch[String])*) : TrieBranch[String] =
    TrieBranch[String](c, Some(word), HashMap(children: _*))

}
