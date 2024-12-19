import java.nio.file.*
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.math.Ordering

object Day12 {
  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/12.txt"), StandardCharsets.UTF_8)

    case class Pos(x: Int, y: Int, plant: Char) {
      def neighbors(map: Vector[Vector[Pos]]): List[Pos] = {
        val xLen = map.head.size
        val yLen = map.size

        (Option.when(x > 0)(map(y)(x - 1)) ++ Option.when(y > 0)(
          map(y - 1)(x)
        ) ++ Option.when(x < xLen - 1)(map(y)(x + 1)) ++ Option
          .when(
            y < yLen - 1
          )(map(y + 1)(x))).toList
          .filter(_.plant == plant)
      }
    }

    val map: Vector[Vector[Pos]] =
      input
        .split("\n")
        .toVector
        .map(_.toCharArray.toVector)
        .zipWithIndex
        .map { case (v, y) =>
          v.zipWithIndex
            .map { case (h, x) => Pos(x, y, h) }
        }

    val xLen = map.head.size
    val yLen = map.size

    @tailrec
    def findRegions(
        elements: Vector[Pos],
        queue: Vector[Pos],
        currentRegion: Vector[Pos],
        regions: Vector[Vector[Pos]]
    ): Vector[Vector[Pos]] = {
      if (queue.isEmpty) {
        if (elements.isEmpty) {
          regions :+ currentRegion
        } else {
          findRegions(
            elements.tail,
            elements.take(1),
            elements.take(1),
            regions :+ currentRegion
          )
        }

      } else {
        val possibleNeighbors = queue.head.neighbors(map).toSet

        val (toQueue, remaining) =
          elements.partition(possibleNeighbors.contains)

        findRegions(
          remaining,
          toQueue ++ queue.tail,
          currentRegion ++ toQueue,
          regions
        )
      }

    }

    val regions =
      map
        .map(_.groupBy(_.plant))
        .reduce((a, b) =>
          (a.toSeq ++ b.toSeq).groupMapReduce(_._1)(_._2)(_ ++ _)
        )
        .view
        .values
        .flatMap(v => findRegions(v.tail, v.take(1), v.take(1), Vector.empty))
        .toVector

    def perimeter(region: Vector[Pos]): Int = {
      (region.size * 4) - region.map(_.neighbors(map).size).sum
    }

    println(regions.map(r => perimeter(r) * r.size).sum)
  }
}
