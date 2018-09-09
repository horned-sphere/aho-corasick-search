package simplesearch.structures

import scala.collection.immutable.HashMap

/**
  * A trie tree of characters. The tree is used to represent a set of words by their prefixes.
  *
  * @see https://en.wikipedia.org/wiki/Trie
  */
object Trie {

  /**
    * A node of a trie tree.
    * @tparam W The type of words represented by the tree.
    */
  sealed trait TrieNode[+W] {

    /**
      * @return The labelled children of the node.
      */
    def children: Map[Char, TrieBranch[W]]

  }

  /**
    * The root of the tree (containing no data).
    * @param children The children of the root.
    * @tparam W The type of words represented by the tree.
    */
  case class TrieRoot[+W](children: HashMap[Char, TrieBranch[W]]) extends TrieNode[W]

  /**
    * A non-root node of the tree.
    * @param label The label of the node.
    * @param dictEntry The word if this node corresponds to a dictionary entry.
    * @param children The labelled children of the node.
    * @tparam W The type of words represented by the tree.
    */
  case class TrieBranch[+W](label: Char,
                            dictEntry: Option[W],
                            children: HashMap[Char, TrieBranch[W]]) extends TrieNode[W]

  /**
    * Recursively update an existing trie with a new word.
    * @param trie The existing trie node.
    * @param word The word being added.
    * @param wordTail The remaining characters of the word to add.
    * @param update Update operation for the node.
    * @tparam W The type of the words.
    * @tparam T The type of the node.
    * @return The updated node.
    */
  private def insertInternal[W, T <: TrieNode[W]](trie: T,
                                                  word: W,
                                                  wordTail: List[Char])
                                                 (update: (T, Char, TrieBranch[W]) => T): T = wordTail match {
    case c :: tail =>
      val replaced = insertInternal(trie.children.getOrElse(
        c, TrieBranch(c, if (tail.isEmpty) Some(word) else None, HashMap.empty)), word, tail)(updateBranch)
      update(trie, c, replaced)
    case _ => trie
  }

  /**
    * Update a trie by replacing one of the children of the root.
    * @param root The root.
    * @param c The first character of the child.
    * @param replacement The replacement node.
    * @tparam W The type of the words.
    * @return The new root.
    */
  private def updateRoot[W](root: TrieRoot[W], c: Char, replacement: TrieBranch[W]): TrieRoot[W] =
    root.copy(children = root.children + (c -> replacement))

  /**
    * Update a trie branch by replacing one of its children.
    * @param branch The branch
    * @param c The first character of the child.
    * @param replacement The replacement node.
    * @tparam W The type of the words.
    * @return The new branch.
    */
  private def updateBranch[W](branch: TrieBranch[W], c: Char, replacement: TrieBranch[W]): TrieBranch[W] =
    branch.copy(children = branch.children + (c -> replacement))

  /**
    * Insert a new word into a trie, producing a new trie.
    * @param rep Representation of the words as strings.
    * @param root The trie.
    * @param word The word to add.
    * @tparam W The type of the words.
    * @return The new updated trie.
    */
  def insert[W](rep: W => String)(root: TrieRoot[W], word: W): TrieRoot[W] =
    insertInternal(root, word, rep(word).toList)(updateRoot)

  /**
    * An empty trie.
    */
  val empty: TrieRoot[Nothing] = TrieRoot[Nothing](HashMap.empty)

  /**
    * Create a trie from a sequence of words.
    * @param words The words.
    * @param rep Representation of the words as strings.
    * @tparam W The type of the words.
    * @return The trie.
    */
  def apply[W](words: Seq[W], rep: W => String): TrieRoot[W] =
    words.foldLeft[TrieRoot[W]](empty)(insert(rep))

  /**
    * Enumerate the nodes of a trie using breadth first search.
    * @param root The trie.
    * @tparam W The type of the words.
    * @return The node of the trie in BFS order.
    */
  def bfsStream[W](root: TrieRoot[W]): Stream[(TrieNode[W], TrieBranch[W])] =
    bfsStream(root.children.values.toStream.map(c => (root, c)))

  private def bfsStream[W](stream: Stream[(TrieNode[W], TrieBranch[W])]): Stream[(TrieNode[W], TrieBranch[W])] = {
    if (stream.isEmpty) stream else {
      val nextLevel = for ((_, node) <- stream;
                           child <- node.children.values.toStream) yield (node, child)

      stream #::: bfsStream(nextLevel)
    }
  }

}


