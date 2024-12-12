import java.nio.file.*
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

object Day09 {
  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/09.txt"), StandardCharsets.UTF_8)
      .trim()

    val empty = -1
    val diskLayout = input.toCharArray.toVector.map(_.toString.toInt)
    val disk = diskLayout.zipWithIndex.flatMap { case (len, index) =>
      if (index % 2 == 0) {
        val id = index / 2
        Array.fill(len)(id)
      } else {
        Array.fill(len)(empty)
      }
    }

    def isDone(disk: Vector[Int]) = {
      val firstEmpty = disk.indexOf(empty)
      disk.drop(firstEmpty).forall(_ == empty)
    }

    @tailrec
    def defrag(disk: Vector[Int]): Vector[Int] = {
      if (isDone(disk)) {
        disk
      } else {
        val firstEmptyIndex = disk.indexOf(empty)
        val lastOccupiedIndex = disk.lastIndexWhere(_ != empty)

        val lastOccupied = disk(lastOccupiedIndex)

        defrag(
          disk
            .updated(firstEmptyIndex, lastOccupied)
            .updated(lastOccupiedIndex, empty)
        )
      }
    }

    def checksum(disk: Vector[Int]): Long = {
      disk.zipWithIndex.map {
        case (`empty`, index) => 0
        case (id, index)      => id.toLong * index
      }.sum
    }

    println(checksum(defrag(disk)))

    enum Area {
      def length: Int

      case Empty(length: Int)
      case File(length: Int, id: Int)
    }

    import Area.*

    val diskAreas = diskLayout.zipWithIndex.flatMap { case (len, index) =>
      if (index % 2 == 0) {
        val id = index / 2
        Some(File(length = len, id = id))
      } else {
        Option.when(len > 0)(Empty(len))
      }
    }

    val maxId = diskAreas.collect { case File(_, id) => id }.max

    def defrag2(disk: Vector[Area], startId: Int): Vector[Area] = {
      if (startId >= 0) {
        val toMovePos = disk.lastIndexWhere {
          case File(length, id) => id == startId
          case _                => false
        }
        val toMove = disk(toMovePos)

        val firstFreePos = disk.indexWhere {
          case Empty(length) if length >= toMove.length => true
          case _                                        => false
        }

        if (firstFreePos == -1 || toMovePos < firstFreePos) {
          defrag2(disk, startId - 1)
        } else {
          val firstFree = disk(firstFreePos)
          val sizeDiff = firstFree.length - toMove.length
          val remainingFree =
            if (sizeDiff > 0) {
              Vector(Empty(sizeDiff))
            } else {
              Vector.empty
            }

          defrag2(
            disk
              .updated(toMovePos, Empty(toMove.length))
              .patch(firstFreePos, toMove +: remainingFree, 1),
            startId - 1
          )
        }
      } else {
        disk
      }
    }

    def diskToBlocks(disk: Vector[Area]): Vector[Int] = {
      disk.flatMap {
        case Empty(length)    => Array.fill(length)(empty)
        case File(length, id) => Array.fill(length)(id)
      }
    }

    println(checksum(diskToBlocks(defrag2(diskAreas, maxId))))
  }
}
