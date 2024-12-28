import java.nio.file.*
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.runtime.Arrays

object Day13 {
  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/13.txt"), StandardCharsets.UTF_8)

    case class Pos(x: Long, y: Long)
    case class Machine(a: Pos, b: Pos, prize: Pos) {
      def solve: Option[(Long, Long)] = {
        val t1 = (b.y * prize.x) - (b.x * prize.y)
        val b1 = (a.x * b.y) - (b.x * a.y)

        val t2 = (a.x * prize.y) - (a.y * prize.x)

        if (t1 % b1 == 0 && t2 % b1 == 0) {
          Some(t1 / b1 -> t2 / b1)
        } else {
          None
        }
      }
    }

    val priceA = 3
    val priceB = 1

    val machines = input.split("\n\n").map { machineS =>
      val lines = machineS.split("\n")
      val buttonPattern = "X\\+(\\d+), Y\\+(\\d+)".r
      val prizePattern = "X=(\\d+), Y=(\\d+)".r

      val aMatches = buttonPattern.findFirstMatchIn(lines(0)).get
      val bMatches = buttonPattern.findFirstMatchIn(lines(1)).get
      val pMatches = prizePattern.findFirstMatchIn(lines(2)).get

      Machine(
        Pos(aMatches.group(1).toLong, aMatches.group(2).toLong),
        Pos(bMatches.group(1).toLong, bMatches.group(2).toLong),
        Pos(pMatches.group(1).toLong, pMatches.group(2).toLong)
      )

    }

    def calc(machines: Iterable[Machine]) = {
      machines
        .map { m =>
          m.solve.map { case (as, bs) =>
            as * priceA + bs * priceB
          }
        }
        .flatten
        .sum
    }

    val results = calc(machines)

    println(results)

    val results2 = calc(
      machines.map(m =>
        m.copy(prize =
          Pos(m.prize.x + 10000000000000L, m.prize.y + 10000000000000L)
        )
      )
    )

    println(results2)
  }
}
