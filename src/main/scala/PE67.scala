import java.io._
import scala.io._

// more of an experiment in making illegal states unrepresentable

object PE67  extends App { // just to run it if you like

  // only make an object if the data is valid
  object NumberTriangle {
    def apply(numbers: Seq[Int]) = {
      if ( math.sqrt( 8 * numbers.size + 1 ).isValidInt)
        new NumberTriangle(numbers)
      else 
        throw new IllegalArgumentException("array of numbers not triangularizable")
    } 
  }
  
  class NumberTriangle private(val nums: Seq[Int]) {
    val numbers = nums
    lazy val triangle: Seq[Seq[Int]] = triangularize(this)
    lazy val maxPath: Int =
      triangle
        .reduceLeft( (bottom, top) => AdjacentLevels(bottom, top).reduceLevel )
	.head
  }
  
  def triangularize(triangle: NumberTriangle): Seq[Seq[Int]] = {
    
    def _triangularize(takeSize:Int, rest: Seq[Int], acc: Seq[Seq[Int]]): Seq[Seq[Int]] = {
      if (rest.size == takeSize)
        rest +: acc
      else
        _triangularize(takeSize + 1, rest.drop(takeSize), rest.take(takeSize) +: acc)
    }
    
    _triangularize(1, triangle.numbers, Seq[Seq[Int]]())
  }

  object AdjacentLevels {
    def apply(bottom: Seq[Int], top: Seq[Int]) = {
      if ( bottom.size - top.size == 1)
        new AdjacentLevels(bottom, top)
      else
        throw new IllegalArgumentException("bottom array of numbers must be one longer than top array")
    }
  }
  
  class AdjacentLevels private(bottom: Seq[Int], top: Seq[Int]) {
    def reduceLevel: Seq[Int] =
      top.zip(bottom.sliding(2).toList).map( x => x._1 + math.max(x._2(0), x._2(1)))
  }
  
  // fetch raw data one way or another
  def fromFile(fName: String): Seq[Int] =
    scala.io.Source.fromFile(fName).mkString.split("\\s+").flatMap(s => if (s == "") Seq() else Seq( s.toInt) )
  
  def pe67(nums: Seq[Int]) = NumberTriangle(nums).maxPath
  
  def writeTriangleToFile = {
    val tr = triangularize( NumberTriangle( fromFile("generated_triangle.txt") ) )
    val strOutput = tr.reverse.map( _.mkString(" ")).mkString("\n")
    val out = new PrintWriter("bigTriangle.txt", "UTF-8") 
    out.print(strOutput)
    out.flush
    out.close
  }
  
  val start = System.currentTimeMillis
  val egNums = fromFile("bigNums.txt")
  val res = pe67(egNums)
  val end = System.currentTimeMillis
  println(res + " \n TOOK " + (end - start))
}
