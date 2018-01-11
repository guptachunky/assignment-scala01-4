import org.apache.log4j.Logger

class Operation {


  def operateList(list: List[Int], operation: String): Int = {

    list match {

      case head :: head1 :: tail if (operation.equals("sum")) => operateList(head + head1 :: tail, operation)
      case head :: head1 :: tail if (operation.equals("product")) => operateList(head * head1 :: tail, operation)
      case head :: head1 :: tail if (operation.equals("max") && head > head1) => operateList(head :: tail, operation)
      case head :: head1 :: tail if (operation.equals("max") && head < head1) => operateList(head1 :: tail, operation)
      case head :: Nil => head

    }
  }

  def sum(f: (Int, Int) => Int, first: Int, second: Int): Int = {

    f(first, second)

  }

}


object OperationTest extends App {

  val log = Logger.getLogger(this.getClass)
  val obj = new Operation
  val first = 1
  val second = 2
  val third = 3
  val fourth = 4
  val myList = List(first, second, third, fourth)
  val sumInt = obj.sum((a, b) => a + b, first, second)
  val sumSquares = obj.sum((a, b) => a * a + b * b, first, second)
  val sumCubes = obj.sum((a, b) => a * a * a + b * b * b, first, second)
  log.info("Sum Of Int =" + sumInt + "\n")
  log.info("Sum Of Squares=" + sumSquares + "\n")
  log.info("Sum Of Cubes" + sumCubes + "\n")
  log.info("Sum Of List" + obj.operateList(myList, "sum") + "\n")
  log.info("Product Of List" + obj.operateList(myList, "product") + "\n")
  log.info("Maximum Of List" + obj.operateList(myList, "max") + "\n")

}
