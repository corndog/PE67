import scala.util._
import scala.util.control.NonFatal

object ProjectEuler67 {
  // collapse the bottom two lines into one
  def reduceLevel(bottom: Seq[Int], top: Seq[Int]): Seq[Int] = {
    if (bottom.size - top.size != 1) {
      println("input file could not be turned be turned into a number triangle")
      System.exit(0)
    }
    top.zip(bottom.sliding(2).toList).map(x => x._1 + math.max( x._2(0), x._2(1)))
  }

  def toListOfNumbers(str: String): Seq[Int] = str.split("\\s+").toList.map(_.toInt)

  // bottom-line-first Seq[Seq[Int]]
  def toNumberTriangle(lines: Seq[String]): Seq[Seq[Int]] = 
    lines.foldLeft(Seq[Seq[Int]]()){ (triangle, line) => toListOfNumbers(line) +: triangle }
  
  def apply(fileName: String) = {
    val start = System.currentTimeMillis

    val result: Try[Int] = for {
      lines <-  Try(scala.io.Source.fromFile(fileName).getLines().toSeq)
      max <- Try(toNumberTriangle(lines).reduceLeft(reduceLevel(_, _)).head)
    } yield max

    result match {
      case Success(maxPath) => println(maxPath + "\nTOOK " + (System.currentTimeMillis - start))
      case Failure(ex: java.io.FileNotFoundException) => println(s"""File "$fileName" not found""")
      case Failure(ex: java.lang.UnsupportedOperationException) => println("File appears to be empty")
      case Failure(NonFatal(ex)) => println(ex.getMessage)
    }
  }
}