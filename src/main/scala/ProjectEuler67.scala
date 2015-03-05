object ProjectEuler67 {
  // this is a somewhat minimalist but still functional-style version
  
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
    val lines = scala.io.Source.fromFile(fileName).getLines().toSeq
    val numberTriangle = toNumberTriangle(lines)
    val maxPath = numberTriangle.reduceLeft(reduceLevel(_, _)).head
    val end = System.currentTimeMillis
    println(maxPath + " \n TOOK " + (end - start))
  }
}