import java.nio.file.*
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

object Day07 {
  @tailrec
  def calculations(
      acc: Set[Long],
      numbers: List[Long],
      operations: List[(Long, Long) => Long]
  ): Set[Long] = {
    numbers match {
      case number :: rest =>
        val results = for {
          result <- acc
          operation <- operations
        } yield {
          operation(result, number)
        }

        calculations(results.toSet, rest, operations)
      case Nil => acc
    }
  }

  def findPossible(
      input: List[(Long, List[Long])],
      operations: List[(Long, Long) => Long]
  ) = {
    input.map { case (expectedResult, numbers) =>
      numbers match {
        case head :: tail =>
          if (
            calculations(Set(head), tail, operations).contains(
              expectedResult
            )
          ) {
            expectedResult
          } else {
            0
          }
        case Nil => ???
      }
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val input: List[(Long, List[Long])] = Files
      .readString(Path.of("input/07.txt"), StandardCharsets.UTF_8)
      .split("\\n")
      .toList
      .map { line =>
        line.split(": ").toList match {
          case resultStr :: numbersStr :: Nil =>
            val expectedResult = resultStr.toLong
            val numbers = numbersStr.split(" ").toList.map(_.toLong)
            (expectedResult, numbers)
          case _ => ???
        }
      }

    val operations: List[(Long, Long) => Long] = List(_ * _, _ + _)
    val possibleSum = findPossible(input, operations)

    println(possibleSum)

    val operations2: List[(Long, Long) => Long] =
      List(_ * _, _ + _, (a, b) => s"$a$b".toLong)
    val possibleSum2 = findPossible(input, operations2)
    println(possibleSum2)
  }
}
