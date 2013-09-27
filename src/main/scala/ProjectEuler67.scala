object ProjectEuler67 {
  // this is a somewhat minimalist but still functional-style version
  
  def reduceLevel(bottom: Seq[Int], top: Seq[Int]): Seq[Int] = {
    if (bottom.size - top.size != 1) {
      println("input file could not be turned be turned into a number triangle")
      System.exit(0)
    }
    top.zip(bottom.sliding(2).toList).map(x => x._1 + math.max( x._2(0), x._2(1)))
  }

  def toListOfNumbers(str: String): Seq[Int] = str.split("\\s+").toList.map(_.toInt)
  
  def main(args: Array[String]) = {
    if (args.size == 0) {
      println("Enter name of file containing number triangle")
    }
    else {
      val start = System.currentTimeMillis
      val numberTriangle = 
        scala.io.Source.fromFile(args(0))
        .getLines()
        .foldLeft(Seq[Seq[Int]]()){ (triangle, line) => toListOfNumbers(line) +: triangle }

      val maxPath = numberTriangle.reduceLeft(reduceLevel(_, _)).head
      val end = System.currentTimeMillis
      println(maxPath + " \n TOOK " + (end - start))
    }
  }
}