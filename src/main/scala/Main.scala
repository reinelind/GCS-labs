import scala.io.Source

object Main extends App {
  val operationsFile = "input.txt"
  val operationList =
    Source.fromFile(operationsFile).getLines.filter(_ != "").toList.map(_.toString.split(" ").toList)
  val uniqueOperations =
    operationList.flatten.toSet
  val operationsCount = operationList.map(_.size).max
  val detailsCount = operationList.size

  def test(headOfList: Option[List[String]], list: List[List[String]], originList: List[List[String]], returnList: List[List[String]])
  : List[List[String]] = originList match {
    case Nil => returnList
    case z :: zs => list match {
      case y :: List(Nil) =>
        List()
      case y :: Nil =>
        test(originList.tail.headOption, originList.tail, originList.tail, returnList)
      case y :: ys =>
        test(headOfList, ys, originList, headOfList.get.intersect(ys.head).union(uniqueOperations.toList.diff(headOfList.get.union(ys.head)))
          :: returnList)
    }
  }

  val result = test(operationList.headOption, operationList, operationList, List())
    .reverse.map(_.size)

  def formResultList(list: List[Int], count: Int): List[List[Int]] = {
    list match {
      case y :: ys => list.take(count) +: formResultList(list.drop(count), count - 1)
      case Nil => Nil
    }
  }

  val resultList = formResultList(result, operationsCount - 1)
  println(resultList)

  val matrix = Array.ofDim[Int](operationsCount, operationsCount)
  for (i <- resultList.indices) {
    matrix(i) = resultList(i).toArray
    println("%" +matrix(i).mkString(" "))
  }


}
