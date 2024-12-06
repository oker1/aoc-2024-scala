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
  }
}
