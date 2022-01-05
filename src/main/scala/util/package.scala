import scala.io.Source

package object util {
  def loadInput(clazz: Class[_]): Source = {
    Source.fromInputStream(clazz.getResourceAsStream(clazz.getSimpleName.dropRight(1) + ".txt"))
  }

  def loadInputLines(clazz: Class[_]): Iterator[String] = {
    loadInput(clazz).getLines()
  }
}