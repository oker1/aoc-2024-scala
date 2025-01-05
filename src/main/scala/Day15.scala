import java.nio.file.*
import java.nio.charset.StandardCharsets

object Day15 {
  case class Pos(x: Int, y: Int)

  def main(args: Array[String]): Unit = {

    val fileParts = Files
      .readString(Path.of("input/15.txt"), StandardCharsets.UTF_8)
      .split("\n\n")

    val initialMap =
      fileParts.head.split("\n").toVector.map(_.toCharArray.toVector)

    val movements = fileParts(1).split("\n").mkString.toCharArray.toVector

    val startPos =
      initialMap.zipWithIndex.flatMap { case (v, y) =>
        v.zipWithIndex
          .flatMap { case (c, x) =>
            Option.when(c == '@')(x)
          }
          .map(x => Pos(x, y))
      }.head

    def move(
        map: Vector[Vector[Char]],
        from: Pos,
        to: Pos
    ): Vector[Vector[Char]] = {
      map
    }

    val (_, finalMap) = movements.foldLeft((startPos, initialMap)) {
      case ((pos, map), movement) =>
        // println(movement)
        // println(map.map(_.mkString).mkString("\n"))

        movement match {
          case '<' =>
            map(pos.y)(pos.x - 1) match {
              case '#' => (pos, map)
              case '.' =>
                (
                  pos.copy(x = pos.x - 1),
                  map.updated(
                    pos.y,
                    map(pos.y).updated(pos.x, '.').updated(pos.x - 1, '@')
                  )
                )
              case 'O' =>
                val nextFreePos =
                  Range(pos.x - 1, 0, -1)
                    .dropWhile(x => map(pos.y)(x) == 'O')
                    .headOption
                    .map(idx => (map(pos.y)(idx), idx))

                nextFreePos match {
                  case Some(('.', idx)) =>
                    val shiftedMap =
                      Range.inclusive(pos.x - 2, idx, -1).foldLeft(map) {
                        case (map, idx) =>
                          map.updated(pos.y, map(pos.y).updated(idx, 'O'))
                      }
                    (
                      pos.copy(x = pos.x - 1),
                      shiftedMap
                        .updated(
                          pos.y,
                          shiftedMap(pos.y)
                            .updated(pos.x, '.')
                            .updated(pos.x - 1, '@')
                        )
                    )
                  case _ => (pos, map)
                }
              case _ => ???
            }
          case '>' =>
            map(pos.y)(pos.x + 1) match {
              case '#' => (pos, map)
              case '.' =>
                (
                  pos.copy(x = pos.x + 1),
                  map.updated(
                    pos.y,
                    map(pos.y).updated(pos.x, '.').updated(pos.x + 1, '@')
                  )
                )
              case 'O' =>
                val nextFreePos =
                  Range(pos.x + 1, map.head.size)
                    .dropWhile(x => map(pos.y)(x) == 'O')
                    .headOption
                    .map(idx => (map(pos.y)(idx), idx))

                nextFreePos match {
                  case Some(('.', idx)) =>
                    val shiftedMap =
                      Range.inclusive(pos.x + 2, idx).foldLeft(map) {
                        case (map, idx) =>
                          map.updated(pos.y, map(pos.y).updated(idx, 'O'))
                      }
                    (
                      pos.copy(x = pos.x + 1),
                      shiftedMap
                        .updated(
                          pos.y,
                          shiftedMap(pos.y)
                            .updated(pos.x, '.')
                            .updated(pos.x + 1, '@')
                        )
                    )
                  case _ => (pos, map)
                }

              case _ => ???
            }
          case '^' =>
            map(pos.y - 1)(pos.x) match {
              case '#' => (pos, map)
              case '.' =>
                (
                  pos.copy(y = pos.y - 1),
                  map
                    .updated(
                      pos.y,
                      map(pos.y).updated(pos.x, '.')
                    )
                    .updated(pos.y - 1, map(pos.y - 1).updated(pos.x, '@'))
                )
              case 'O' =>
                val nextFreePos =
                  Range(pos.y - 1, 0, -1)
                    .dropWhile(y => map(y)(pos.x) == 'O')
                    .headOption
                    .map(idx => (map(idx)(pos.x), idx))

                nextFreePos match {
                  case Some(('.', idx)) =>
                    val shiftedMap =
                      Range.inclusive(pos.y - 2, idx, -1).foldLeft(map) {
                        case (map, idx) =>
                          map.updated(idx, map(idx).updated(pos.x, 'O'))
                      }
                    (
                      pos.copy(y = pos.y - 1),
                      shiftedMap
                        .updated(
                          pos.y,
                          shiftedMap(pos.y).updated(pos.x, '.')
                        )
                        .updated(
                          pos.y - 1,
                          shiftedMap(pos.y - 1).updated(pos.x, '@')
                        )
                    )
                  case _ => (pos, map)
                }
              case _ => ???
            }
          case 'v' =>
            map(pos.y + 1)(pos.x) match {
              case '#' => (pos, map)
              case '.' =>
                (
                  pos.copy(y = pos.y + 1),
                  map
                    .updated(
                      pos.y,
                      map(pos.y).updated(pos.x, '.')
                    )
                    .updated(pos.y + 1, map(pos.y + 1).updated(pos.x, '@'))
                )
              case 'O' =>
                val nextFreePos =
                  Range(pos.y + 1, map.size)
                    .dropWhile(y => map(y)(pos.x) == 'O')
                    .headOption
                    .map(idx => (map(idx)(pos.x), idx))

                nextFreePos match {
                  case Some(('.', idx)) =>
                    val shiftedMap =
                      Range.inclusive(pos.y + 2, idx).foldLeft(map) {
                        case (map, idx) =>
                          map.updated(idx, map(idx).updated(pos.x, 'O'))
                      }
                    (
                      pos.copy(y = pos.y + 1),
                      shiftedMap
                        .updated(
                          pos.y,
                          shiftedMap(pos.y).updated(pos.x, '.')
                        )
                        .updated(
                          pos.y + 1,
                          shiftedMap(pos.y + 1).updated(pos.x, '@')
                        )
                    )
                  case _ => (pos, map)
                }
              case _ => ???
            }
          case _ => ???
        }
    }

    println("")
    println(finalMap.map(_.mkString).mkString("\n"))

    val sum = (for {
      y <- Range(0, finalMap.size)
      x <- Range(0, finalMap.head.size)
    } yield {
      if (finalMap(y)(x) == 'O') { 100 * y + x }
      else {
        0
      }
    }).sum
    println(sum)
  }
}
