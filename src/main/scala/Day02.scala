import java.nio.file.*
import java.nio.charset.StandardCharsets

object Day02 {
  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/02.txt"), StandardCharsets.UTF_8)

    val data = input.split("\n").toList.map(_.split(" ").toList.map(_.toInt))

    def isSafe(line: List[Int]): Boolean = {
      val diffs = line
        .zip(line.tail)
        .map { case (x, y) => y - x }

      (
        diffs.forall(_ > 0) || diffs.forall(
          _ < 0
        )
      ) && diffs.forall(x => x.abs <= 3)
    }

    val result1 = data.map(line => if isSafe(line) then 1 else 0).sum

    println(result1)

    val result2 = data.map { line =>
      val possibleFixedVersions = line.zipWithIndex.map { case (_, index) =>
        line.patch(index, List.empty, 1)
      }

      if (possibleFixedVersions.exists(isSafe)) {
        1
      } else {
        0
      }
    }.sum

    println(result2)
  }
}
