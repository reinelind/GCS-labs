import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot._
import scalax.collection.io.dot.implicits._
import scalax.collection.Graph
import scalax.collection.edge.LDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.io.dot._
import implicits._

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
  val groups = getGroups(resultMatrix, result.distinct.sortWith(_ > _), result.distinct.sortWith(_ > _).head, detailsCountSet, res).filter(_.nonEmpty)

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

  val lab1Result = groupsStr(groups).filter(_.nonEmpty)

  println("Groups:\n" + lab1Result.map(_.mkString(" ")).mkString("\n"))
  println("\n" + groups.map(_.mkString(" ")).mkString("\n"))

  val lab1ResultSorted = lab1Result.sortWith(_.size > _.size)
  val groupsAndObjects = lab1Result.zip(groups).sortWith(_._1.size > _._1.size)


  println("\nSorted groups: \n" + lab1ResultSorted.map(_.mkString(" ")).mkString("\n"))
  //  println ("\n"+groupsAndObjects.map(_._2.mkString(" ")).mkString ("\n"))

  def getRefinedGroups(l: List[(List[String], List[Int])], originList: List[Int] = 1.to(operationList.size).toList)
  : List[(List[String], List[Int])] = {
    def groupsContain(lst: List[String]): (List[String], List[Int]) = {
      var res: List[Int] = Nil
      for (i <- 0 until originList.size) {
        if (operationList(originList(i) - 1).forall(lst.contains))
          res ::= originList(i)
      }
      (groupsStr(List(res)).flatten, res)
    }

    (l, originList) match {
      case (y :: ys, x :: xs) =>
        val result = groupsContain(l.head._1)
        List(result) ::: getRefinedGroups(ys, originList.filterNot(result._2.contains))

      case (y :: ys, Nil) =>
        List()
      case (_, _) =>
        List()
    }
  }


  println("Fixed groups:\n" + getRefinedGroups(groupsAndObjects).map(_._1.mkString(" ")).mkString("\n"))
  println("Fixed objects:\n" + getRefinedGroups(groupsAndObjects).map(_._2.mkString(" ")).mkString("\n"))



  def getListOfOperations (groupsIndices : List[List[Int]]) : List [List[List[String]]] = {
    def getListFlat (indices : List[Int]) : List[List[String]] = indices match {
      case y :: ys => operationList (y-1) :: getListFlat (ys)
      case Nil => Nil
    }
    groupsIndices match {
      case x :: xs => getListFlat(x) :: getListOfOperations(xs)
      case Nil => List()
    }

  }

  val lstOfOperations = getListOfOperations(getRefinedGroups(groupsAndObjects).map(_._2.sortWith(_ < _)))
  println (lstOfOperations)

  def getListOfEdges(operations: List [List[List[String]]]): List[List[(String, String)]] = {

    def getGroups (groups : List[List[String]]) : List[List[(String,String)]] = {
      def getNodes(list: List[String]): List[(String, String)] = {
        list match {
          case y :: Nil =>
            Nil
          case Nil =>
            Nil
          case y :: ys =>
            (y,ys.head) :: getNodes (ys)

        }
      }
      groups match {
        case g :: gs =>
          getNodes (g) :: getGroups (gs)
        case Nil =>
          List(Nil)
      }
    }
    operations match {
      case y :: ys  =>
        getGroups (y) ::: getListOfEdges(ys)
      case Nil =>
        Nil
    }
  }

  val listOfEdges = getListOfEdges(lstOfOperations).flatten

  def initializeMatrix (edgesList : List[(String, String)], gr : Graph [String, LDiEdge])
  : Graph [String, LDiEdge] = edgesList match {
    case y :: ys =>
      initializeMatrix(ys,gr + (y._1 ~+> y._2)(""))
    case Nil =>
      gr
  }

  val resultLab3 = initializeMatrix(listOfEdges, Graph())
  println (initializeMatrix(listOfEdges, Graph()))


  val root = DotRootGraph (directed = true, id = None)

  def edgeTransformer(innerEdge: Graph[String,LDiEdge]#EdgeT):
  Option[(DotGraph,DotEdgeStmt)] = innerEdge.edge match {
    case LDiEdge(source, target, label) => label match {
      case label: String =>
        Some((root,
          DotEdgeStmt(source.toString,
            target.toString,
            if (label.nonEmpty) List(DotAttr("label", label.toString))
            else                Nil)))
    }}


  val dot = resultLab3.toDot (root,edgeTransformer)

  import sys.process._

  val file = new File ("lab3.dot")
  val bw = new BufferedWriter (new FileWriter(file))
  bw.write (dot)
  bw.close()

  val terminalCommands = List ("dot -Tpng -O lab3.dot", "xdg-open lab3.dot.png")

  terminalCommands.foreach (_ !)


}



