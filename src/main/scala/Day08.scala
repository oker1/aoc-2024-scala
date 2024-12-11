import java.nio.file.*
import java.nio.charset.StandardCharsets

object Day08 {
  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/08.txt"), StandardCharsets.UTF_8)

    case class Antenna(x: Int, y: Int, freq: Char)

    val map = input.split("\n").toVector.map(_.toCharArray.toVector)

    val xLen = map.head.size
    val yLen = map.size

    val antennaLocations = map.zipWithIndex.flatMap { case (v, y) =>
      v.zipWithIndex
        .flatMap { case (c, x) =>
          Option.when(c != '.')((x, c))
        }
        .map[Antenna] { case (x, f) => Antenna(x, y, f) }
    }

    val antennaPairs =
      antennaLocations
        .groupBy(_.freq)
        .view
        .mapValues(_.combinations(2).toVector)
        .values
        .toVector
        .flatten

    def antiNodes(pair: Vector[Antenna]) = {
      pair.toList match {
        case a1 :: a2 :: Nil =>
          val dx = a1.x - a2.x
          val dy = a1.y - a2.y

          val an1 = Antenna(a1.x + dx, a1.y + dy, a1.freq)
          val an2 = Antenna(a2.x - dx, a2.y - dy, a1.freq)
          Vector(an1, an2).filter(a =>
            a.x >= 0 && a.x < xLen && a.y >= 0 && a.y < yLen
          )
        case _ => ???
      }
    }

    println(antennaPairs.flatMap(antiNodes).map(a => (a.x, a.y)).distinct.size)

    def antiNodes2(pair: Vector[Antenna]) = {
      pair.toList match {
        case a1 :: a2 :: Nil =>
          val dx = a1.x - a2.x
          val dy = a1.y - a2.y

          for {
            i <- Range(
              1,
              math.max(
                math.ceil(xLen.toDouble / dx.abs.toDouble).toInt,
                math.ceil(yLen.toDouble / dy.abs.toDouble).toInt
              )
            )
            an1 = Antenna(a1.x + (dx * i), a1.y + (dy * i), a1.freq)
            an2 = Antenna(a2.x - (dx * i), a2.y - (dy * i), a1.freq)
            a <- Vector(an1, an2, a1, a2)
            if a.x >= 0 && a.x < xLen && a.y >= 0 && a.y < yLen
          } yield {
            a
          }
        case _ => ???
      }
    }

    println(antennaPairs.flatMap(antiNodes2).map(a => (a.x, a.y)).distinct.size)
  }
}
