package a2021

object A12 extends App {
  val lines = util.loadInputLines(getClass)

  val r = "(.*?)-(.*)".r

  val edges = lines
    .flatMap { case r(a, b) => List(a -> b, b -> a) }
    .toList
    .groupMap(_._1)(_._2)

  val normalisedEdges = edges
    .map { case (from, tos) => (from, tos
      .flatMap(to => {
        if (to.forall(_.isUpper)) {
          edges(to)
        } else {
          List(to)
        }
      })
      .filter(_ != "start") //can't go back to the start
    )
    }
    .filter(_._1.forall(!_.isUpper))

  def findRoutes(routes: List[(Boolean, List[String])]): List[(Boolean, List[String])] = {
    val (complete, incomplete) = routes.partition("end" == _._2.head)
    if (incomplete.isEmpty) {
      complete
    } else {
      val newIncomplete = incomplete.flatMap { case (hasLoop, route) =>
        if ("end" == route.head) {
          List((hasLoop, route))
        } else {
          normalisedEdges(route.head)
            .map { n => (route.contains(n), n :: route) }
            .filterNot(hasLoop && _._1) //If we already had a loop, we can't have another
            .map(t => (hasLoop || t._1, t._2)) //Need to remember if we already had a loop previously
        }
      }
      complete ++ findRoutes(newIncomplete)
    }
  }

  println(findRoutes(List((true, List("start")))).size)
  println(findRoutes(List((false, List("start")))).size)
}
