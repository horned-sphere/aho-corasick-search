package simplesearch

import java.io._

import simplesearch.algorithm.ConceptSearch

/**
  * Application to demonstrate the [[simplesearch.algorithm.ConceptSearch]].
  */
object SimpleSearchApp {


  def main(args : Array[String]): Unit = {
    if (args.length != 2) {
      println("Correct parameters:")
      println("CONCEPTS_FILE SENTENCES_FILE")
    } else {

      val concepts = new File(args(0))
      val sentences = new File(args(1))

      //Attempt to read in the concepts to search for.

      val conceptsIn = new FileInputStream(concepts)
      val search = try {
        new ConceptSearch(linesFromStream(conceptsIn))
      } finally {
        conceptsIn.close()
      }

      //Attempt to read in the sentences of interest and output the concepts therein.

      val sentencesIn = new FileInputStream(sentences)
      try {
        linesFromStream(sentencesIn).foreach { line =>
          val concepts = search.conceptsIn(line)
          println(line)
          println(s"Concepts: $concepts")
          println()
        }
      } finally {
        sentencesIn.close()
      }
    }
  }

  /**
    * Read the lines from a file as a stream.
    * @param in The stream.
    * @return The stream of lines.
    */
  def linesFromStream(in : InputStream) : Stream[String] = {

    val reader= new BufferedReader(new InputStreamReader(in))

    def lines : Stream[String] = {
      val line = reader.readLine()
      if (line == null) Stream.empty else line #:: lines
    }
    lines
  }

}
