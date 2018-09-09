package simplesearch.algorithm

import org.scalatest.{FunSpec, Matchers}

/**
  * Unit tests for [[ConceptSearch]].
  */
class ConceptSearchTest extends FunSpec with Matchers {

  import ConceptSearch._
  import ConceptSearchTest._

  describe("The phrase pre-processing function") {

    it("should replace white space and non-word, non-digit characters with a single space," +
      "bracket with spaces and convert to lower case") {

      preprocessPhrase("word") shouldEqual " word "
      preprocessPhrase("Word") shouldEqual " word "
      preprocessPhrase("two Words") shouldEqual " two words "
      preprocessPhrase(" word") shouldEqual " word "
      preprocessPhrase("wOrD2") shouldEqual " word2 "
      preprocessPhrase("double-barrelled 'with' punctuation.") shouldEqual " double barrelled with punctuation "

    }
  }

  describe("The concept detector") {

    it("should find the expected concepts in a list of test sentences") {
       val cs = new ConceptSearch(TestConcepts)

      for ((sentence, expected) <- TestSentences.zip(ExpectedConcepts)) {

        cs.conceptsIn(sentence) shouldEqual expected

      }

    }

  }

}

object ConceptSearchTest {

  /**
    * Concepts to use for validation.
    */
  val TestConcepts = List(
    "Indian",
    "Thai",
    "Sushi",
    "Caribbean",
    "Italian",
    "West Indian",
    "Pub",
    "East Asian",
    "BBQ",
    "Chinese",
    "Portuguese",
    "Spanish",
    "French",
    "East European"
  )

  /**
    * Sentences to use for validation.
    */
  val TestSentences = List(
    "I would like some thai food",
    "Where can I find good sushi",
    "Find me a place that does tapas",
    "Which restaurants do East Asian food",
    "Which restaurants do West Indian food",
    "What is the weather like today"
  )

  /**
    * The concepts that we expect to find.
    */
  val ExpectedConcepts = List(
    Set("Thai"),
    Set("Sushi"),
    Set.empty[String],
    Set("East Asian"),
    Set("Indian", "West Indian"),
    Set.empty[String]
  )

}
