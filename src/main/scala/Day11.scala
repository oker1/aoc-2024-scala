import java.nio.file.*
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

object Day11 {
  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/11.txt"), StandardCharsets.UTF_8)

    val stones = input.split(" ").toVector.map(_.toLong)

    def blink(stones: Vector[Long], count: Int): Vector[Long] = {
      Range(0, count).foldLeft(stones) { case (s, i) =>
        s.flatMap { stone =>
          stone.toString match {
            case "0" => Vector(1L)
            case n if n.length % 2 == 0 =>
              n.splitAt(n.length / 2).toList.toVector.map(_.toLong)
            case _ => Vector(stone * 2024L)
          }
        }
      }
    }

    println(blink(stones, 25).size)

    case class Stones(iterationsLeft: Int, stones: Vector[Long])

    @tailrec
    def blinkDepthFirst(
        stones: Stones,
        queue: Vector[Stones],
        acc: Int
    ): Int = {
      if (stones.iterationsLeft == 0) {
        queue.headOption match {
          case None => acc + stones.stones.size
          case Some(head) =>
            blinkDepthFirst(head, queue.tail, acc + stones.stones.size)
        }
      } else {
        val stone = stones.stones.head
        val rest = stones.stones.tail
        val next = stone.toString match {
          case "0" => Vector(1L)
          case n if n.length % 2 == 0 =>
            n.splitAt(n.length / 2).toList.toVector.map(_.toLong)
          case _ => Vector(stone * 2024L)
        }

        val toQueue = Option
          .when(next.tail.nonEmpty)(
            Stones(stones.iterationsLeft - 1, next.tail)
          )
          .toVector ++ Option
          .when(rest.nonEmpty)(
            Stones(stones.iterationsLeft, rest)
          )
          .toVector ++ queue

        blinkDepthFirst(
          Stones(stones.iterationsLeft - 1, next.take(1)),
          toQueue,
          acc
        )
      }

    }

    println(blinkDepthFirst(Stones(25, stones), Vector.empty, 0))
  }
}
