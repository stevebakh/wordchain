package substrate

import scala.io.Source.fromResource
import scala.io.StdIn.readLine

object WordchainGame extends App {
  val solver = new Wordchain(new Dictionary {
    override lazy val words: Set[String] = fromResource("dictionary.txt").mkString.split(',').toSet
  })

  println("Wordchain - select a start and end word to find the chain.")

  while (true) {
    print("Start word: ")
    val from: String = readLine()

    print("End word: ")
    val to: String = readLine()

    try {
      println(solver.chain(from, to)
        .map(_.mkString(" > "))
        .getOrElse(s"No chain possible between '$from' and '$to'"))
    } catch {
      case e: Exception => println(s"Error! ${e.getMessage}")
    }
  }
}
