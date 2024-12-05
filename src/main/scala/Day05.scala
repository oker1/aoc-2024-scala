import java.nio.file.Files
import java.nio.file.Path
import java.nio.charset.StandardCharsets

object Day05 {
  def main(args: Array[String]): Unit = {
    case class Rule(page: Int, after: Int)

    val fileParts = Files
      .readString(Path.of("input/05.txt"), StandardCharsets.UTF_8)
      .split("\n\n")

    val rules = fileParts(0)
      .split("\n")
      .toList
      .map { s =>
        s.split("\\|", 2).toList.map(_.toInt)
      }
      .map {
        case page :: after :: Nil =>
          Rule(page, after)
        case _ => ???
      }

    val updates =
      fileParts(1).split("\n").map(_.split(",").toList.map(_.toInt)).toList

    val groupedRules = rules.groupBy(_.page)

    def validate(update: List[Int]) = {
      update.foldLeft((Vector.empty[Int], true)) {
        case ((before, isValid), page) =>
          if (!isValid) {
            (before, isValid)
          } else {
            groupedRules.get(page) match {
              case None =>
                (before :+ page, isValid)
              case Some(rulesForPage) =>
                (
                  before :+ page,
                  rulesForPage.forall(r => before.forall(_ != r.after))
                )
            }

          }
      }
    }

    val result1 = updates.map { update =>
      val res = validate(update)
      if (res._2) {
        update(update.length / 2)
      } else {
        0
      }
    }.sum

    println(result1)

    val result2 = updates.map { update =>
      val res = validate(update)
      if (!res._2) {
        val relevantRules = rules.filter(rule =>
          update.contains(rule.page) && update.contains(rule.after)
        )

        val rulesByAfter =
          relevantRules.groupBy(_.after).view.mapValues(_.toSet).toMap

        def sortByRules(
            acc: List[Int],
            rules: Map[Int, Set[Rule]],
            update: Set[Int]
        ): List[Int] = {
          if (update.isEmpty) {
            acc
          } else {
            update.find(page =>
              !rules.contains(page) || rules.get(page) == Some(Set())
            ) match {
              case Some(page) =>
                sortByRules(
                  page :: acc,
                  rules.map { case (p, rules) =>
                    p -> rules.filter(_.page != page)
                  },
                  update - page
                )
              case None => ???
            }
          }
        }

        val fixedUpdate =
          sortByRules(List.empty, rulesByAfter, update.toSet).reverse

        fixedUpdate(fixedUpdate.length / 2)
      } else {
        0
      }
    }.sum

    println(result2)
  }
}
