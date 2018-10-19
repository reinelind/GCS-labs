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

  val detailsCountList = (1 to detailsCount).toList.sortWith(_ > _)
  val detailsCountSet  = detailsCountList.toSet
  val res = resultMatrix.map(_.zipWithIndex).zipWithIndex.map (x => (x._1.map(y => (y._1, (x._2 + 2)+y._2 ))
    .filter(_._1 == 10), x._2 + 1 ))
    .filter(_._1.nonEmpty).map(res => (res._1.map(_._2),res._2))

  println (res)

//  def getGroups (matrix : List[List[Int]], count : List[Int], numOfSet : Int, set : Set[Int]
//                 , result : List[(List[Int], Int)]) :List [(List[Int],Int)] = {
//    set.toList match {
//      case x :: Nil =>
//        List((List(-1),x))
//      case x :: xs =>
//        result match {
//          case Nil =>
//            count match {
//              case z :: zs =>
//
//            }
//        }
//    }
//
//
//  }
//
  def getGroups (matrix : List[List[Int]], count : List[Int], numOfSet : Int, set : Set[Int], result
  : List[(List[Int],Int)] ) : List[(List[Int],Int)] = {
    set.toList.sortWith(_ > _) match {
      case x :: Nil =>
        List ((List(-1), x))
      case x :: xs =>
        result match {
          case Nil =>
            count match {
              case z :: zs =>
              val res = resultMatrix.map(_.zipWithIndex).zipWithIndex.map(x => (x._1.map(y => (y._1, (x._2 + 2) + y._2))
                  .filter(_._1 == count.tail.headOption.getOrElse(-1)), x._2 + 1))
                  .filter(_._1.nonEmpty).map(res => (res._1.map(_._2),res._2))
                getGroups(matrix, count.tail, count.tail.headOption.getOrElse(-1), set, res)
              case Nil => List()
            }
              case y :: ys =>
//            if (set.contains(y._1) && set.contains(y._2))
//              y :: getGroups(matrix,count,numOfSet,set.filter(x => x != y._1 && x != y._2),ys)
                val r = (y._1.toSet.union(Set(y._2)).intersect (set.diff(Set(y._2))).toList, y._2)
                r :: getGroups(matrix,count,numOfSet, set.diff(y._1.toSet.union(Set(y._2))), ys)
//            else
//              getGroups(matrix,count,numOfSet,set,ys)
        }

      case Nil => List()
    }
  }

// println(getGroups (resultMatrix, result.distinct.sortWith(_>_), result.distinct.sortWith(_>_).head,detailsCountSet, res)
//   .filter(_._1.nonEmpty))

}

//  (x=> (x._1.filter(y=>(y._1 == 11)),x._2)).filter(_._1.nonEmpty))


