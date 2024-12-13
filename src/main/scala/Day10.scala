import java.nio.file.*
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

object Day10 {
  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/10.txt"), StandardCharsets.UTF_8)

    case class Pos(x: Int, y: Int, height: Int)

    val map =
      input
        .split("\n")
        .toVector
        .map(_.toCharArray.toVector)
        .zipWithIndex
        .map { case (v, y) =>
          v.zipWithIndex
            .map { case (h, x) => Pos(x, y, h.toString.toInt) }
        }

    val xLen = map.head.size
    val yLen = map.size

    val possibleTrailheads = map.map(_.filter(_.height == 0)).flatten

    @tailrec
    def findTrails(pos: Pos, queue: List[Pos], acc: Set[Pos]): Set[Pos] = {
      if (pos.height == 9) {
        queue match {
          case Nil          => acc + pos
          case head :: tail => findTrails(head, tail, acc + pos)
        }
      } else {
        val possibleNeighbors = List(
          (pos.x - 1, pos.y),
          (pos.x + 1, pos.y),
          (pos.x, pos.y - 1),
          (pos.x, pos.y + 1)
        ).filter(p => p._1 >= 0 && p._1 < xLen && p._2 >= 0 && p._2 < yLen)
          .map { case (x, y) => map(y)(x) }
          .filter(_.height == pos.height + 1)

        possibleNeighbors match {
          case Nil =>
            queue match {
              case Nil          => acc
              case head :: tail => findTrails(head, tail, acc)
            }
          case head :: tail =>
            findTrails(head, tail ++ queue, acc)
        }
      }
    }

    val result =
      possibleTrailheads.map(findTrails(_, List.empty, Set.empty).size).sum

    println(result)

    @tailrec
    def findTrails2(pos: Pos, queue: List[Pos], acc: Int): Int = {
      if (pos.height == 9) {
        queue match {
          case Nil          => acc + 1
          case head :: tail => findTrails2(head, tail, acc + 1)
        }
      } else {
        val possibleNeighbors = List(
          (pos.x - 1, pos.y),
          (pos.x + 1, pos.y),
          (pos.x, pos.y - 1),
          (pos.x, pos.y + 1)
        ).filter(p => p._1 >= 0 && p._1 < xLen && p._2 >= 0 && p._2 < yLen)
          .map { case (x, y) => map(y)(x) }
          .filter(_.height == pos.height + 1)

        possibleNeighbors match {
          case Nil =>
            queue match {
              case Nil          => acc
              case head :: tail => findTrails2(head, tail, acc)
            }
          case head :: tail =>
            findTrails2(head, tail ++ queue, acc)
        }
      }
    }

    val result2 =
      possibleTrailheads.map(findTrails2(_, List.empty, 0)).sum

    println(result2)

  }
}
