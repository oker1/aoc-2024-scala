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

    val startPos =
      map.zipWithIndex.flatMap { case (v, y) =>
        v.zipWithIndex
          .flatMap { case (c, x) =>
            Option.when(c == '^')(x)
          }
          .map(x => x -> y)
      }.head

    val xLen = map.head.size
    val yLen = map.size

    val steps = Stream.unfold((startPos, Up)) {
      case a @ (pos @ (x, y), direction) =>
        if (x < 0 || y < 0 || x == xLen || y == xLen) {
          None
        } else {
          direction match {
            case Up =>
              if (y == 0) {
                Some((pos, ((x, y - 1), Up)))
              } else {
                if (map(y - 1)(x) == '#') {
                  Some((pos, (pos, Right)))
                } else {
                  Some((pos, ((x, y - 1), Up)))
                }
              }
            case Left =>
              if (x == 0) {
                Some((pos, ((x - 1, y), Left)))
              } else {
                if (map(y)(x - 1) == '#') {
                  Some((pos, (pos, Up)))
                } else {
                  Some((pos, ((x - 1, y), Left)))
                }
              }
            case Down =>
              if (y == yLen - 1) {
                Some((pos, ((x, y + 1), Down)))
              } else {
                if (map(y + 1)(x) == '#') {
                  Some((pos, (pos, Left)))
                } else {
                  Some((pos, ((x, y + 1), Down)))
                }
              }
            case Right =>
              if (x == xLen - 1) {
                Some((pos, ((x + 1, y), Right)))
              } else {
                if (map(y)(x + 1) == '#') {
                  Some((pos, (pos, Down)))
                } else {
                  Some((pos, ((x + 1, y), Right)))
                }
              }
          }
        }
    }

    println(steps.toList.distinct.size)
  }
}
