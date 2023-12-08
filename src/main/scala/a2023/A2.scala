package a2023

object A2 extends App {
  val lines = util.loadInputLines(getClass).toList

  val linePattern = "Game (\\d+):(.*)".r
  val rPattern = "\\s*(\\d+) red".r
  val gPattern = "\\s*(\\d+) green".r
  val bPattern = "\\s*(\\d+) blue".r

  //Process the list of games to get a List(Int, List(Int, Int, Int))
  val games = lines
    .map {
      //Extract the gameNumber and the hands using regex
      case linePattern(gameNumber, rest) =>
        val hands = rest
          //Split to get each hand
          .split(";")
          .toList
          //Map each hand to a tuple of r, g, b
          .map(game =>
            game
              .split(",")
              //Map each colour to a tuple describing that colour only, using regex
              .map {
                case rPattern(red) => (red.toInt, 0, 0)
                case gPattern(green) => (0, green.toInt, 0)
                case bPattern(blue) => (0, 0, blue.toInt)
              }
              //Sum the tuples for each hand
              .reduce((t1, t2) => (t1._1 + t2._1, t1._2 + t2._2, t1._3 + t2._3))
          )
        //Return the game number and the list of hands
        (gameNumber.toInt, hands)
    }

  val max = (12, 13, 14)

  val sum1 = games
    //Filter for only games that contain exclusively possible hands
    .filter { case (_, hands) => !hands
      //i.e. none must exist that exceed the maximum for each colour
      .exists(hand => hand._1 > max._1 || hand._2 > max._2 || hand._3 > max._3)
    }
    .map(game => game._1)
    .sum

  println(sum1)

  val sum2 = games
    .map(_._2)
    //Reduce the hands, finding the maximal number of each cube
    .map(_.reduce((h1, h2) => (h1._1.max(h2._1), h1._2.max(h2._2), h1._3.max(h2._3))))
    .map(minHand => minHand._1 * minHand._2 * minHand._3)
    .sum

  println(sum2)

}