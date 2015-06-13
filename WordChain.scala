package puzzle

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import com.rockymadden.stringmetric.similarity.LevenshteinMetric


trait WordChainSolver {
  def chain(from: String, to: String): Option[List[String]]
}


object WordChain extends WordChainSolver {

  case class Word(value: String, parent: Option[Word] = None)

  private val file = scala.io.Source.fromFile("dictionary.txt")
  private val dictionary = file.mkString.split(',').toSet
  file.close


  // a poor man's graph... is it considered cheating to do this up front?
  // I initially did this work on-demand, but it was a lot less performant.
  // Uses Levenshtein distance to reduce the set of related words.
  private val graphish: Map[String, Set[String]] = dictionary.map(word =>
    word -> dictionary.filter(filteredWord =>
      // comment out the following line to allow chains between words of different lengths
      filteredWord.length == word.length &&
      LevenshteinMetric.compare(word, filteredWord).getOrElse(0) == 1)).toMap


  def chain(from: String, to: String): Option[List[String]] = {
    @tailrec def walkWordParents(word: Word, list: List[String] = List.empty): List[String] = {
      val currentList = list :+ word.value
      word.parent match {
        case Some(parent) => walkWordParents(parent, currentList)
        case None => currentList
      }
    }

    // should guarantee the shortest path between the words...
    @tailrec def breadthFirstSearch(queue: Queue[Word], visited: Set[String] = Set.empty): List[String] = {
      if (queue.isEmpty)
        List.empty
      else {
        val (candidateWord, headless) = queue.dequeue

        if (candidateWord.value == to)
          walkWordParents(candidateWord).reverse
        else {
          // To the words in the existing queue, add the immediate descendants of the current
          // word, excluding those that are already in the queue, or have already been visited.
          val updatedQueue = headless ++ (graphish(candidateWord.value) -- visited -- queue.map(_.value).toSet)
            .map(child => Word(child, Some(candidateWord)))

          breadthFirstSearch(updatedQueue, visited + candidateWord.value)
        }
      }
    }

    breadthFirstSearch(Queue(Word(from))) match {
      case l: List[String] if l.nonEmpty => Some(l)
      case _ => None
    }
  }
}


object Exercise extends App {
  printChain("cold", "warm")
  printChain("blue", "pink")
  printChain("ape", "man")
  printChain("lead", "gold")

  // The following examples will only work if you comment
  // out the documented line where the graph is generated.
  // Doing so will enable chains between words of different
  // lengths, but comes with a slight performance cost (both
  // at the point of generating the graphs and during search)
  printChain("cat", "kite")
  printChain("at", "home")

  def printChain(from: String, to: String) = {
    WordChain.chain(from, to) match {
      case None => println(s"Unable to find a chain between '$from' and '$to'!")
      case Some(chain) => println(s"from: '$from' to: '$to' in ${chain.size - 1} steps: ${chain.mkString(" > ")}")
    }
  }
}
