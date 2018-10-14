import scala.io.Source

object Main extends App {
  val operationList =
    Source.fromFile("input.txt").getLines.filter(_ != "").toList.map(_.toString.split(" ").toList)
  val uniqueOperations = operationList.flatten.toSet
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
      case y :: ys => list.take(count-1) +: formResultList(list.drop(count-1), count - 1)
      case Nil => Nil
    }
  }
  lazy val resultMatrix = formResultList(result, operationList.size)
  lazy val padding = "%1$" ++ s"${resultMatrix.map (_.mkString (" ")).head.length}s"

  resultMatrix.map (_.mkString (" ")).foreach(x=> println (padding.format(x)))

  println (resultMatrix.map(_.zipWithIndex).zipWithIndex.map (x => (x._1.map(y => (y._1, (x._2 + 1)+y._2 )).filter(_._1 == 10), x._2 ))
    .filter(_._1.nonEmpty))

//  (x=> (x._1.filter(y=>(y._1 == 11)),x._2)).filter(_._1.nonEmpty))

}
