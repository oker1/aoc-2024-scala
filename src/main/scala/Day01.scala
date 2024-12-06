import java.nio.file.*
import java.nio.charset.StandardCharsets

object Day01 {
  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/01.txt"), StandardCharsets.UTF_8)

    val (left, right) = input
      .split("\n")
      .toList
      .map(_.split("   ").toList)
      .map {
        case l :: r :: Nil => (l.toInt, r.toInt)
        case _             => ???
      }
      .unzip

    val result1 = left.sorted.zip(right.sorted).map(e => (e._1 - e._2).abs).sum

    println(result1)

    val rightAppearances =
      right.groupBy(identity).view.mapValues(_.length).toMap

    val result2 = left.map(v => v * rightAppearances.getOrElse(v, 0)).sum

    println(result2)
  }
}
