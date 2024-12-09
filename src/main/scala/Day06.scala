import java.nio.file.*
import java.nio.charset.StandardCharsets

object Day06 {

  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/06.txt"), StandardCharsets.UTF_8)

    val map = input.split("\n").toVector.map(_.toCharArray.toVector)

    enum Direction {
      case Up, Left, Down, Right
    }
    import Direction.*

    enum Elem {
      case Out
      case Pos(x: Int, y: Int, direction: Direction)
    }
    import Elem.*

    val startPos =
      map.zipWithIndex.flatMap { case (v, y) =>
        v.zipWithIndex
          .flatMap { case (c, x) =>
            Option.when(c == '^')(x)
          }
          .map[Pos](x => Pos(x, y, Up))
      }.head

    val xLen = map.head.size
    val yLen = map.size

    def steps(map: Vector[Vector[Char]], startPos: Pos) = {
      import Elem.*
      import Direction.*
      Stream.unfold(startPos: Elem) {
        case Out =>
          None
        case pos @ Pos(x, y, direction) =>
          if (x < 0 || y < 0 || x == xLen || y == xLen) {
            Some(Out, Out)
          } else {
            direction match {
              case Up =>
                if (y == 0) {
                  Some((pos, Pos(x, y - 1, Up)))
                } else {
                  if (map(y - 1)(x) == '#') {
                    Some((pos, pos.copy(direction = Right)))
                  } else {
                    Some((pos, Pos(x, y - 1, Up)))
                  }
                }
              case Left =>
                if (x == 0) {
                  Some((pos, Pos(x - 1, y, Left)))
                } else {
                  if (map(y)(x - 1) == '#') {
                    Some((pos, pos.copy(direction = Up)))
                  } else {
                    Some((pos, Pos(x - 1, y, Left)))
                  }
                }
              case Down =>
                if (y == yLen - 1) {
                  Some((pos, Pos(x, y + 1, Down)))
                } else {
                  if (map(y + 1)(x) == '#') {
                    Some((pos, (pos.copy(direction = Left))))
                  } else {
                    Some((pos, Pos(x, y + 1, Down)))
                  }
                }
              case Right =>
                if (x == xLen - 1) {
                  Some((pos, Pos(x + 1, y, Right)))
                } else {
                  if (map(y)(x + 1) == '#') {
                    Some((pos, (pos.copy(direction = Down))))
                  } else {
                    Some((pos, Pos(x + 1, y, Right)))
                  }
                }
            }
          }
      }
    }

    println(
      steps(map, startPos).collect { case Pos(x, y, _) => (x, y) }.distinct.size
    )

    def isLoop(map: Vector[Vector[Char]], startPos: Pos) = {
      import Elem.*
      import Direction.*

      val stream =
        Stream.unfold((steps(map, startPos), Set.empty[Pos], false)) {
          case (stepStream, seenPositions, end) =>
            if (end) {
              None
            } else {
              stepStream match {
                case Out #:: _ => None
                case (head: Pos) #:: tail =>
                  if (seenPositions.contains(head)) {
                    Some((true, (tail, seenPositions, true)))
                  } else {
                    Some((false, (tail, seenPositions + head, false)))
                  }
                case Stream.Empty => None
              }
            }
        }

      stream.find(_ == true).isDefined
    }

    val loopCount = (for {
      y <- Range(0, yLen)
      x <- Range(0, xLen)
    } yield {
      if (map(y)(x) == '.') {
        isLoop(map.updated(y, map(y).updated(x, '#')), startPos)
      } else {
        false
      }
    }).count(_ == true)

    println(loopCount)
  }

}
