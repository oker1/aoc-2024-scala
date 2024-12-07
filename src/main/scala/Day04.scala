import java.nio.file.*
import java.nio.charset.StandardCharsets

object Day04 {
  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/04.txt"), StandardCharsets.UTF_8)

    val chars = input.split("\n").toVector.map(_.toCharArray.toVector)

    val lineLength = chars.head.size

    val xmas = "XMAS".toCharArray.toVector
    val needles =
      Array(xmas, xmas.reverse)

    val possibleLocations = (for {
      x <- Range.inclusive(0, lineLength - xmas.length)
      y <- Range.inclusive(0, chars.size - xmas.length)
      l <- List(
        Vector((x, y), (x + 1, y), (x + 2, y), (x + 3, y)),
        Vector((x, y), (x, y + 1), (x, y + 2), (x, y + 3)),
        Vector((x, y), (x + 1, y + 1), (x + 2, y + 2), (x + 3, y + 3)),
        Vector((x + 3, y), (x + 2, y + 1), (x + 1, y + 2), (x, y + 3))
      )
    } yield { l }) ++ (for {
      x <- Range.inclusive(lineLength - xmas.length + 1, lineLength - 1)
      y <- Range.inclusive(0, chars.size - xmas.length)
    } yield {
      Vector((x, y), (x, y + 1), (x, y + 2), (x, y + 3))
    }) ++ (for {
      x <- Range.inclusive(0, lineLength - xmas.length)
      y <- Range.inclusive(chars.size - xmas.length + 1, chars.size - 1)
    } yield {
      Vector((x, y), (x + 1, y), (x + 2, y), (x + 3, y))
    })

    val result = possibleLocations
      .map(_.map { case (a, b) =>
        chars(a)(b)
      })
      .count(needles.contains)

    println(result)

    val needles2 =
      Vector("MMASS", "MSAMS", "SSAMM", "SMASM").map(_.toCharArray.toVector)

    val possibleLocations2 = for {
      x <- Range.inclusive(0, lineLength - 3)
      y <- Range.inclusive(0, chars.size - 3)
    } yield {
      Vector((x, y), (x + 2, y), (x + 1, y + 1), (x, y + 2), (x + 2, y + 2))
    }
    val result2 = possibleLocations2
      .map(_.map { case (a, b) =>
        chars(a)(b)
      })
      .count(needles2.contains)

    println(result2)
  }
}
