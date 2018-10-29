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
      case y :: ys => list.take(count - 1) +: formResultList(list.drop(count - 1), count - 1)
      case Nil => Nil
    }
  }

  lazy val resultMatrix = formResultList(result, operationList.size)
  lazy val padding = "%1$" ++ s"${resultMatrix.map(_.mkString(" ")).head.length}s"

  resultMatrix.map(_.mkString(" ")).foreach(x => println(padding.format(x)))

  val detailsCountList = (1 to detailsCount).toList.sortWith(_ > _)
  val detailsCountSet = detailsCountList.toSet
  val res = resultMatrix.map(_.zipWithIndex).zipWithIndex.map(x => (x._1.map(y => (y._1, (x._2 + 2) + y._2))
    .filter(_._1 == result.distinct.sortWith(_ > _).head), x._2 + 1))
    .filter(_._1.nonEmpty).flatMap(res => (res._1.map(_._2 :: List(res._2))))


  def func(l: List[List[Int]], set: Set[Int]): List[List[Int]] = {
    l match {
      case y :: ys =>
        if (y.forall(set.contains))
          y :: func(ys, set)
        else
          func(ys, set)
      case Nil =>
        List()
    }
  }

  def getGroups(matrix: List[List[Int]], count: List[Int], numOfSet: Int, set: Set[Int], result
  : List[List[Int]]): List[List[Int]] = {
    set.toList.sortWith(_ > _) match {
      case x :: Nil =>
        List(List(x))
      case x :: xs =>
        result match {
          case Nil =>
            count match {
              case z :: zs =>
                val res = resultMatrix.map(_.zipWithIndex).zipWithIndex.map(x => (x._1.map(y => (y._1, (x._2 + 2) + y._2))
                  .filter(_._1 == count.tail.headOption.getOrElse(-1)), x._2 + 1))
                  .filter(_._1.nonEmpty).flatMap(res => res._1.map(_._2 :: List(res._2)))
                getGroups(matrix, count.tail, count.tail.headOption.getOrElse(-1), set, res)
              case Nil => List()
            }
          case y :: ys =>
            def func(l: List[List[Int]], set: Set[Int]): List[List[Int]] = {
              l match {
                case y :: ys =>
                  if (y.forall(set.contains))
                    y :: func(ys, set)
                  else
                    func(ys, set)
                case Nil =>
                  List()
              }
            }

            val r = func(result, set)
            List(r.flatten.distinct) ::: getGroups(matrix, count, numOfSet, set.diff(r.flatten.toSet), Nil)
        }

      case Nil => List()
    }
  }

  //
  val groups = getGroups(resultMatrix, result.distinct.sortWith(_ > _), result.distinct.sortWith(_ > _).head, detailsCountSet, res)

  def groupsStr(groups: List[List[Int]]): List[List[String]] = {
    def groupsOperationList(el: List[Int], result: List[String]): List[String] = {
      el match {
        case y :: ys =>
          if (result.isEmpty)
            groupsOperationList(ys, operationList(y - 1))
          else
            groupsOperationList(ys, result.toSet.union(operationList(y - 1).toSet).toList)
        case Nil =>
          result
      }
    }

    groups match {
      case y :: ys =>
        groupsOperationList(y, Nil) :: groupsStr(groups.tail)
      case Nil =>
        List()
    }
  }

  val lab1Result = groupsStr(groups)

  println("Groups:\n" + lab1Result.map(_.mkString(" ")).mkString("\n"))
  println("\n" + groups.map(_.mkString(" ")).mkString("\n"))

  val lab1ResultSorted = lab1Result.sortWith(_.size > _.size)
  val groupsAndObjects = lab1Result.zip (groups).sortWith (_._1.size > _._1.size)


  println("\nSorted groups: \n" + lab1ResultSorted.map(_.mkString(" ")).mkString("\n"))
  //  println ("\n"+groupsAndObjects.map(_._2.mkString(" ")).mkString ("\n"))

  def getRefinedGroups(l: List[(List[String],List[Int])], originList : List[Int] = 1.to (operationList.size).toList)
  : List[(List[String],List[Int])] = {
    def groupsContain (lst: List[String]) : (List[String],List[Int]) = {
      var res : List[Int] = Nil
      for (i <-0 until originList.size)
        {
          if (operationList(originList(i)-1).forall(lst.contains))
            res::=originList(i)
        }
      (groupsStr(List(res)).flatten,res)
    }
    (l, originList) match {
    case (y :: ys, x :: xs) =>
      val result = groupsContain (l.head._1)
      List(result) ::: getRefinedGroups(ys, originList.filterNot(result._2.contains))

    case (y::ys, Nil) =>
      List()
    case (_, _) =>
        List()
  }
  }



  println ("Fixed groups:\n"+getRefinedGroups(groupsAndObjects).map(_._1.mkString(" ")).mkString("\n"))
  println ("Fixed objects:\n"+getRefinedGroups(groupsAndObjects).map(_._2.mkString(" ")).mkString("\n"))


}

//  (x=> (x._1.filter(y=>(y._1 == 11)),x._2)).filter(_._1.nonEmpty))


