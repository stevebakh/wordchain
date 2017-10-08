package substrate

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.immutable.Queue

class Wordchain(dictionary: Dictionary) {

  private def validateInput(from: String, to: String): Unit = {
    if (from.length != to.length)
      throw new IllegalArgumentException("Start and end word must be the same length")

    if (!dictionary.words.contains(from))
      throw new InvalidWordException(s"word '$from' not contained in dictionary")

    if (!dictionary.words.contains(to))
      throw new InvalidWordException(s"word '$to' not contained in dictionary")
  }

  def chain(from: String, to: String): Option[Seq[String]] = {
    validateInput(from, to)

    case class Candidate(word: String, parent: Option[Candidate] = None)

    @tailrec def walkGraph(last: Candidate, chain: Seq[String] = Seq.empty): Seq[String] = {
      val updatedChain = last.word +: chain
      last.parent match {
        case Some(parent) => walkGraph(parent, updatedChain)
        case None => updatedChain
      }
    }

    @tailrec def breadthFirstSearch(candidates: Queue[Candidate],
                                    visited: Set[String] = Set.empty): Option[Seq[String]] = {
      if (candidates.isEmpty)
        None
      else {
        val candidate = candidates.head

        if (candidate.word == to)
          Some(walkGraph(candidate))
        else {
          def singleCharacterDifference(word: String): Boolean =
            word.length == candidate.word.length && word.zip(candidate.word).count { case (l, r) => l != r } == 1

          val updatedCandidates = candidates.tail ++
            (dictionary.words.par.filter(singleCharacterDifference).seq -- visited -- candidates.map(_.word))
              .map(Candidate(_, Some(candidate)))

          breadthFirstSearch(updatedCandidates, visited + candidate.word)
        }
      }
    }

    breadthFirstSearch(Queue(Candidate(from)))
  }
}
