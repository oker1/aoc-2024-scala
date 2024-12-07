import java.nio.file.*
import java.nio.charset.StandardCharsets

object Day03 {
  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/03.txt"), StandardCharsets.UTF_8)

    val regex = "mul\\((\\d+),(\\d+)\\)".r

    val result1 = regex
      .findAllMatchIn(input)
      .toList
      .map(m => m.group(1).toInt * m.group(2).toInt)
      .sum

    println(result1)

    enum Operation {
      case Do
      case Dont
      case Mul(x: Int, y: Int)
    }
    import Operation.*

    val operations =
      "(do\\(\\)|don't\\(\\)|mul\\((\\d+),(\\d+)\\))".r
        .findAllMatchIn(input)
        .toList
        .map { m =>
          m.matched match {
            case "don't()" => Dont
            case "do()"    => Do
            case s if s.startsWith("mul(") =>
              Mul(m.group(2).toInt, m.group(3).toInt)
            case _ => ???
          }
        }

    val result2 = operations
      .foldLeft((List.empty[Mul], true)) { case ((muls, doing), op) =>
        op match
          case Do   => (muls, true)
          case Dont => (muls, false)
          case mul @ Mul(x, y) if doing =>
            (mul :: muls, doing)
          case _: Mul => (muls, doing)
      }
      ._1
      .map(m => m.x * m.y)
      .sum

    println(result2)
  }
}
