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

    println(blink(stones.take(1), 25).size)

    case class Stone(value: Long, count: Int) {
      def next: List[Stone] = value.toString match {
        case "0" => List(Stone(1L, count - 1))
        case n if n.length % 2 == 0 =>
          n.splitAt(n.length / 2).toList.map(v => Stone(v.toLong, count - 1))
        case _ => List(Stone(value * 2024L, count - 1))
      }

    }
    enum History {
      case Single(child1: Stone)
      case Branch(child1: Stone, child2: Stone)
      case PartialBranch(child1Res: Long, child2: Stone)
    }

    var memo = Map.empty[Long, Map[Int, Long]]

    def memoize(s: Stone, v: Long) = {
      memo = memo.updated(
        s.value,
        memo.getOrElse(s.value, Map.empty).updated(s.count, v)
      )
    }

    // @tailrec
    def blinkDepthFirst(
        stone: Stone,
        queue: List[Stone],
        history: List[History]
    ): Long = {
      // println(s"s: $stone q: $queue h: $history")

      val memoized = memo.get(stone.value).flatMap(_.get(stone.count))

      if (stone.count == 1 || memoized.nonEmpty) {
        val res = memoized.getOrElse(stone.next.size.toLong)
        memo = memo.updated(
          stone.value,
          memo.getOrElse(stone.value, Map.empty).updated(stone.count, res)
        )

        if (queue.nonEmpty) {
          val (toDiscard, toKeep) = history.span {
            case History.Branch(a, b) if b == queue.head => false
            case _                                       => true
          }

          memoize(stone, res)

          val sum = toDiscard.foldLeft(res) {
            case (acc, History.PartialBranch(pres, b)) =>
              memoize(b, acc)

              acc + pres
            case (acc, History.Single(b)) =>
              memoize(b, acc)

              acc
          }

          val nextHistory = (toKeep.head match {
            case History.Branch(a, b) =>
              memoize(a, sum)
              History.PartialBranch(sum, b)
          }) :: toKeep.tail

          blinkDepthFirst(queue.head, queue.tail, nextHistory)
        } else {
          history.collect { case History.PartialBranch(res, _) =>
            res
          }.sum + res
        }
      } else {
        val next = stone.next

        val historyHead = next match {
          case head :: Nil         => History.Single(head)
          case head :: next :: Nil => History.Branch(head, next)
          case _                   => ???
        }

        blinkDepthFirst(
          next.head,
          next.tail ++ queue,
          historyHead :: history
        )

      }
    }

    println(
      stones.map(s => blinkDepthFirst(Stone(s, 75), List.empty, List.empty)).sum
    )
  }
}
