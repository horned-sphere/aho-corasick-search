package simplesearch.algorithm

import simplesearch.structures.Trie
import simplesearch.structures.Trie.{TrieBranch, TrieNode, TrieRoot}

/**
  * Functional implementation of the Aho-Corasick algorithm for efficiently finding instances
  * of a set of strings within a longer string.
  *
  * The algorithm works by constructing a finite state machine whose state tracks the prefixes
  * of words that have been observed at the current point in the input. At each state transition
  * the complete words that are matched at that point are output.
  *
  * @see https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm
  * @param trie Trie tree describing the prefixes of words in the search set.
  * @param maxSuffixes Links from nodes in the trie the their longest proper suffix in the tree. This
  *                    will be the root in the worst case.
  * @param dictSuffixes Links from nodes in the tree the their longest proper suffix in the tree which
  *                     corresponds to one of the search words.
  * @tparam W The type of the search words.
  */
class AhoCorasick[W] private (trie : TrieRoot[W],
                              maxSuffixes : Map[TrieNode[W], TrieNode[W]],
                              dictSuffixes : Map[TrieNode[W], TrieNode[W]]) {

  import AhoCorasick._

  /**
    * List all occurrences of the phrases encoded in the trie in the order that they occur in a sentence.
    * @param sentence The sentence.
    * @return The occurrences.
    */
  def findPhrasesIn(sentence : String): List[W] = {

    val start : (TrieNode[W], List[W]) = (trie, Nil)

    val (_, outputs) = sentence.foldLeft(start) { (acc, c) =>
      acc match {
        case (state, results) =>
          //Select the next state for the FSM based on the next character.
          val next = stateTransition(trie, c, state, maxSuffixes)
          val newResults = next match {
            //Find all words that match at this state.
            case branch : TrieBranch[W] => outputsAt(branch, dictSuffixes).reverse ::: results
            case _ => results
          }
          (next, newResults)
      }
    }
    //Output list is collected in reverse order of occurrence.
    outputs.reverse
  }

}

object AhoCorasick {

  /**
    * Construct an instance of [[AhoCorasick]] from a dictionary of words.
    * @param phrases The dictionary.
    * @param rep Representation of the words as strings of characters.
    * @tparam W The type of the words.
    * @return The [[AhoCorasick]] instance.
    */
  def apply[W](phrases : Seq[W], rep : W => String) : AhoCorasick[W] = {
    val trie = Trie(phrases, rep)
    val maxSuffixes = computeMaximalSuffixes(trie)
    val dictSuffixes = computeDictionarySuffixes(trie, maxSuffixes)
    new AhoCorasick(trie, maxSuffixes, dictSuffixes)
  }

  /**
    * Find the longest suffixes of each node in a trie.
    * @param trie The trie.
    * @tparam W The type of the words.
    * @return Map from nodes to their longest suffixes.
    */
  def computeMaximalSuffixes[W](trie : TrieRoot[W]) : Map[TrieNode[W], TrieNode[W]] = {

    //To find the longest suffix of a node it is sufficient to have the longest suffixes of all nodes
    //higher in the tree. Therefore, it is safe to construct them by breadth first search.
    Trie.bfsStream(trie).foldLeft(Map[TrieNode[W], TrieNode[W]]()) { (partialSuffixes, entry) =>
      entry match {
        case (parent, child) =>
          val suffix = findSuffix(child.label, parent, partialSuffixes).getOrElse(trie)
          partialSuffixes + (child -> suffix)
      }
    }
  }

  /**
    * Find the longest suffix of each node that exists in the dictionary in a trie.
    * @param trie The trie.
    * @param maxSuffixes The maximal suffixes in general.
    * @tparam W The type of the words.
    * @return The longest suffixes that are in the dictionary.
    */
  def computeDictionarySuffixes[W](trie : TrieRoot[W],
                                   maxSuffixes : Map[TrieNode[W], TrieNode[W]]) : Map[TrieNode[W], TrieNode[W]] = {

    val dictSuffixes = for (node <- maxSuffixes.keys) yield {

      //Traverse through the suffixes until one is found that is in the dictionary.
      val dictSuffix = traverse(node, maxSuffixes).collectFirst {
        case target @ TrieBranch(_, Some(_), _) => target
      }.getOrElse(trie)

      node -> dictSuffix
    }

    dictSuffixes.toMap
  }

  /**
    * Find the longest suffix for a single node.
    * @param c The last character at the node of interest.
    * @param parent The parent of the current node.
    * @param partial The longest suffixes for all nodes higher in the tree.
    * @tparam W The type of the words.
    * @return The longest suffix, if it exists.
    */
  private def findSuffix[W](c : Char, parent : TrieNode[W],
                         partial : Map[TrieNode[W], TrieNode[W]]) : Option[TrieNode[W]] = {
    traverse(parent, partial).find(_.children.contains(c)).map(_.children(c))
  }

  /**
    * Traverse a structure using links provided by a map. If the map contains cycles this stream
    * will be infinite.
    * @param node The start node in the structure.
    * @param links Map linking locations in the structure.
    * @tparam T The type of the elements.
    * @return Stream that traverses the structure.
    */
  private def traverse[T](node : T, links : Map[T, T]) : Stream[T] = {
    links.get(node) match {
      case Some(next) => next #:: traverse(next, links)
      case _ => Stream.empty
    }
  }

  /**
    * List all the outputs that match at a node in a trie.
    * @param node The node.
    * @param dictSuffixes The dictionary suffixes for all nodes in the trie.
    * @tparam W The type of the words.
    * @return The matches.
    */
  def outputsAt[W](node : TrieBranch[W], dictSuffixes : Map[TrieNode[W], TrieNode[W]]) : List[W] = {
    //Find all the suffixes that match.
    val suffixes = traverse(node, dictSuffixes).toList.collect {
      case TrieBranch(_, Some(word), _) => word
    }
    //If the current node is also in the dictionary, include it.
    node.dictEntry match {
      case Some(word) => word :: suffixes
      case _ => suffixes
    }
  }

  /**
    * State transition function for the Aho-Corasick state machine.
    * @param root The trie describing the FSM.
    * @param data The current character of input.
    * @param current The current state.
    * @param maxSuffixes The longest suffixes for each node of the trie.
    * @tparam W The type of the words.
    * @return The next state.
    */
  private def stateTransition[W](root : TrieRoot[W],
                         data : Char,
                         current : TrieNode[W],
                                 maxSuffixes : Map[TrieNode[W], TrieNode[W]]) : TrieNode[W] = {
    current.children.get(data) match {
      case Some(child) => child
      case _ => traverse(current, maxSuffixes)
        .find(_.children.contains(data)).map(_.children(data))
        .getOrElse(root)
    }
  }
}
