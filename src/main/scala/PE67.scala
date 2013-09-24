object PE67 extends App { // just to run it if you like

  object NumberTriangle {
    def apply(numbers: Seq[Int]) = {
      if ( math.sqrt( 8 * numbers.size + 1 ).isValidInt)
        new NumberTriangle(numbers)
      else 
        throw new IllegalArgumentException("array of numbers not triangularizable")
    } 
  }
  
  class NumberTriangle private(val nums: Seq[Int]) {
    lazy val maxPath: Int =
      triangularize
        .reduceLeft( (bottom, top) => AdjacentLevels(bottom, top).reduceLevel )
	.head
    
    private def triangularize: Seq[Seq[Int]] = {
    
      def _triangularize(takeSize:Int, rest: Seq[Int], acc: Seq[Seq[Int]]): Seq[Seq[Int]] = {
        if (rest.size == takeSize)
          rest +: acc
        else
          _triangularize(takeSize + 1, rest.drop(takeSize), rest.take(takeSize) +: acc)
      }
    
      _triangularize(1, nums, Seq[Seq[Int]]())
    }
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
    scala.io.Source.fromFile(fName).mkString.split("\\s+").map(_.toInt)
  
  def pe67(nums: Seq[Int]) = NumberTriangle(nums).maxPath
  
  // example usage
  val egNums = Seq(9, 2, 1, 1, 5, 7, 9, 5, 3, 3)
  println(pe67(egNums))
}
