import java.nio.file.*
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.runtime.Arrays

object Day14 {
  def main(args: Array[String]): Unit = {
    val input = Files
      .readString(Path.of("input/14.txt"), StandardCharsets.UTF_8)

    val xSize = 101
    val ySize = 103

    case class Pos(x: Long, y: Long)
    case class Robot(position: Pos, velocity: Pos) {
      def move(count: Int): Robot = {
        val x = (position.x + count * velocity.x) % xSize
        val y = (position.y + count * velocity.y) % ySize

        val x2 = if (x < 0) x + xSize else x
        val y2 = if (y < 0) y + ySize else y

        Robot(Pos(x2, y2), velocity)
      }

      def quadrant: Option[Int] = {
        val xBoundary = (xSize / 2)
        val yBoundary = (ySize / 2)

        if (position.x < xBoundary) {
          if (position.y < yBoundary) {
            Some(0)
          } else if (position.y > yBoundary) {
            Some(1)
          } else {
            None
          }
        } else if (position.x > xBoundary) {
          if (position.y < yBoundary) {
            Some(2)
          } else if (position.y > yBoundary) {
            Some(3)
          } else {
            None
          }
        } else {
          None
        }

      }
    }
    object Pos {
      def apply(parts: String): Pos = {
        parts.split(",").toList match {
          case x :: y :: Nil => Pos(x.toLong, y.toLong)
          case _             => ???
        }
      }
    }

    val robots = input.split("\n").toList.map { line =>
      line.split(" ").toList match {
        case p :: v :: Nil =>
          val pos = Pos(p.stripPrefix("p="))
          val vel = Pos(v.stripPrefix("v="))
          Robot(pos, vel)
        case _ => ???
      }
    }

    println(
      robots
        .groupMapReduce(_.move(100).quadrant)(_ => 1)(_ + _)
        .view
        .filter(_._1.isDefined)
        .values
        .reduce(_ * _)
    )
  }
}
