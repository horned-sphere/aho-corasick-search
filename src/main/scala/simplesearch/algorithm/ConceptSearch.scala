package simplesearch.algorithm

/**
  * Finds all occurrences of a number of concepts within a string.
  *
  * <ol>
  *   <li>A word is a sequence of letter and digit characters.</li>
  *   <li>A concept is a sequence of words separated by white space.</li>
  *   <li>Punctuation in concepts and sentences is treated as if it did not exist.</li>
  *   <li>Any quantity of white space is treated the same as a single space.</li>
  *   <li>Concepts are not case sensitive.</li>
  *   <li>A concept occurs in a sentence if its words occur, in order surrounded by white space or attached to
  *   the ends of the sentence.</li>
  * </ol>
  * @param concepts The list of concepts.
  */
class ConceptSearch(concepts : Seq[String]) {

  import ConceptSearch._

  /**
    * Aho-Corasick state machine to detect the concepts.
    */
  private val searchFsm: AhoCorasick[String] = AhoCorasick[String](concepts, preprocessPhrase)

  /**
    * Find the concepts occurring in a sentence.
    * @param sentence The sentence.
    * @return The set of concepts.
    */
  def conceptsIn(sentence : String): Set[String] = searchFsm.findPhrasesIn(preprocessPhrase(sentence)).toSet


}

object ConceptSearch {

  /**
    * Regular expression to detect all characters that are not letters and digits.
    */
  private val IgnoreRegex = "[^\\p{L}\\p{Nd}]+".r

  /**
    * Pre-process a phrase (either a concept or sentence) for use in the detector. All sequences of non-letter, non-digit
    * characters are replaced by a single white space. All letters are changed to lower case. The string is padded with
    * a single space at either end to allow matches to be found the edges of sentences and prevent run-on matches of
    * concepts.
    * @param phrase The phrase to be pre-processed.
    * @return The pre-processed phrase.
    */
  def preprocessPhrase(phrase : String) : String =
    IgnoreRegex.replaceAllIn(s" $phrase ", " ").toLowerCase

}
