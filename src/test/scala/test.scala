import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import PE67._

// in sbt  test:run
object AdjacentLevelsTest {

  def run = {
  
    val propCheckSimpleReduce = forAll { (n: Int, m: Int, p:Int) =>
      val top = Seq(p)
      val bottom = Seq(n, m)
      AdjacentLevels(bottom, top).reduceLevel.head == p + math.max(n, m)
    }
    
    val propCheckValidAdjacent = forAll { n:Int =>
      (n > 0 && n < 100) ==> ( AdjacentLevels( (1 to n+1), (1 to n)).reduceLevel.size == (1 to n).size)
    }
    
    //val testFail = forAll { n:Int =>
    //	n + 2 == n * 2
    //}
    
    propCheckSimpleReduce.check
    propCheckValidAdjacent.check
    
    //testFail.check
  }
  
  //def main(args: Array[String]) = {
  //  run
  //}
}

object TriangleTest {

  def sumToN(n: Int) = n * (n+1) / 2

  def run = {
    val propValidTriangularization = forAll { n:Int =>
      (n > 0 && n < 500) ==> NumberTriangle(1 to sumToN(n)).maxPath > 0
    }
    
    propValidTriangularization.check
  }
}

object allTests extends App {
  AdjacentLevelsTest.run
  TriangleTest.run
}