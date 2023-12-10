package a2023

object A5 extends App {
  val lines = util.loadInputLines(getClass).toList

  trait Boundary {
    def from: Long
    def to: Long
  }

  case class Start(from: Long, to: Long) extends Boundary
  case class End(from: Long, to: Long) extends Boundary
  case class Mapping(start: Start, end: End)

  //Parse the seeds into a List[Long]
  val seeds = lines.head.substring(7).split(" ").map(_.toLong).toList

  //Parse the input into a list of lists of Mappings
  val groups = lines
    .slice(2, lines.size - 1)
    .mkString("\n")
    .split("\n\n")
    .map(
      _.split("\n")
        .tail
        .map(_.split(" ").map(_.toLong).toList match {
          //Resolve the length into the mapping end, so we don't have to bother later
          case List(a, b, c) => Mapping(Start(b, a), End(b + c - 1, a + c - 1))
        })
        .toList
        .sortBy(_.start.from)
        //Fill in the gaps in source
    )
    .toList

  def mapFully(seedRanges: List[(Long, Long)]): List[(Long, Long)] = {
    groups.foldLeft(seedRanges)((acc, group) => {
      acc
        .flatMap(r => {
          //Compute a flat list of mapped range boundaries, book-ended by boundaries for
          //the start and end of the input range
          //We're building the list backwards, so start with the end
          val end = List[Boundary](End(r._2, r._2))
          //Then add the boundaries for each relevant mapping
          val mappingsAndEnd = group
            //find mappings that affect this range
            .filter(m =>
              //start of the mapping is before the end of the range
              m.start.from <= r._2 &&
              //end of the mapping is after the start of the range
              m.end.from >= r._1
            )
            //Make sure to foldRight, as we're building the list backwards
            .foldRight(end)(
              (m, acc) => {
                val start =
                  if (m.start.from < r._1) {
                    val startDiff = r._1 - m.start.from
                    Start(m.start.from + startDiff, m.start.to + startDiff)
                  } else {
                    m.start
                  }

                val end =
                  if (m.end.from > r._2) {
                    val endDiff = m.end.from - r._2
                    End(m.end.from - endDiff, m.end.to - endDiff)
                  } else {
                    m.end
                  }

                start :: end :: acc
              }
            )

          //Append the start
          val boundaries = Start(r._1, r._1) :: mappingsAndEnd

          //Zip each boundary with the next one
          boundaries
            .zip(boundaries.tail)
            .map {
              //Adjacent starts only occurs at the start of the range
              case (Start(start1From, _), Start(start2From, _)) => (start1From, start2From - 1)
              //Mapped region
              case (Start(startFrom, startTo), End(endFrom, endTo)) => (startTo, (endFrom - startFrom) + startTo)
              //Between mapped regions
              case (End(endFrom, _), Start(startFrom, _)) => (endFrom + 1, startFrom - 1)
              //Adjacent ends only occurs at the end of the range
              case (End(end1From, _), End(end2From, _)) => (end1From + 1, end2From)
            }
            //Ranges must describe at least 1 element
            .filter(newR => newR._1 <= newR._2)
        })
        .sortBy(_._1)
    })
  }

  val mappedSeeds = mapFully(seeds
    //map each seed into a range describing that seed only
    .sorted
    .map(s => (s, s))
  )

  println(mappedSeeds.head._1)

  val mappedRanges = mapFully(seeds
    .grouped(2)
    .map { case List(start, length) => (start, start + length - 1) }
    .toList
  )

  println(mappedRanges.head._1)
}