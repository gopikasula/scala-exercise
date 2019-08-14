package exercise

object Assignment2 extends App {
  val text = "iam iam gopi krishna hi hi hey hey"

  def wordCountDesc(text: String): List[(String, Int)] = {
    text.split(" ")
      .groupBy(x => x)
      .mapValues(words => words.length)
      .toList
      .sortWith((t1:(String,Int),t2:(String,Int))=>
        if(t1._2 > t2._2) true
        else if(t1._2 == t2._2) t1._1 < t1._1
        else false
      )
  }

  println(wordCountDesc(text))

  //reverse the list
  def reversefoldLeft(list: List[Int]): List[Int] =
    list.foldLeft(List[Int]())((acc, ele) => ele +: acc)

  //reverse of list using tailrec
  def reverse(list: List[Int]): List[Int] = {
    @tailrec
    def reverseAux(list: List[Int], acc: List[Int]): List[Int] =
      if (list.isEmpty) acc else reverseAux(list.tail, list.head +: acc)

    reverseAux(list, List[Int]())
  }

  sealed trait Expr
  case class Number(i: Int) extends Expr
  case class Sum(expr1: Expr, expr2: Expr) extends Expr
  case class Subtract(expr1: Expr, expr2: Expr) extends Expr
  case class Multiply(expr1: Expr, expr2: Expr) extends Expr

  def eval(expr: Expr): Int = {
    expr match {
      case Number(n) => n
      case Sum(expr1, expr2) => eval(expr1) + eval(expr2)
      case Subtract(expr1, expr2) => eval(expr1) - eval(expr2)
      case Multiply(expr1, expr2) => eval(expr1) * eval(expr2)
    }
  }

  println(eval(Sum(Number(2), Subtract(Number(5), Number(4)))))

  case class MyList(list: List[Int]) {
    def map(f: Int => Int): List[Int] = {
      list.foldLeft(List[Int]())((acc, ele) => acc :+ f(ele))
    }

    def flatMap(f: Int => List[Int]): List[Int] = {
      list.foldLeft(List[Int]())((acc, ele) => acc ++ f(ele))
    }
  }

  println(MyList(List(1, 2, 3)).map(x => x))
  println(MyList(List(1, 2, 3)).flatMap((x) => List(x)))

  def fuse[A,B](a:Option[A], b:Option[B]): Option[(A, B)] =
    a.flatMap(a => b.map(b => (a,b)))

  println(fuse(Some(1),None))

  def  composeFunction[A,B,C](g:B=>C, f:A=> B):A=>C= f andThen g

  def window(length: Int, list: List[Int]): List[List[Int]] = {
    def windowAux(list: List[Int], acc: List[List[Int]]): List[List[Int]] =
      if (list.length < length) acc else windowAux(list.drop(length), acc :+ list.take(length))
    windowAux(list, List[List[Int]]())
  }


  println(window(2, List(1, 2, 3, 4, 5, 6)))

  def myZip(list1: List[Int], list2: List[String]): List[(Int, String)] = {
    if (list1.length < list2.length) list1.indices.toList.map(index => (list1(index), list2(index)))
    else list2.indices.toList.map(index => (list1(index), list2(index)))
  }

  println(myZip(List(1, 2, 3), List("a", "b", "c", "d")))

  def reduce(list: List[Map[String, Int]]): Map[String, Int] =
    list.foldLeft(Map[String, Int]())((acc, ele) => acc ++ ele)

  def reduce2(list: List[Map[String, Map[String, Int]]]): Map[String, Map[String, Int]] =
    list.foldLeft(Map[String, Map[String, Int]]())((acc, ele) => acc ++ ele)

  def merge(map1: Map[String, Int], map2: Map[String, Int]): Map[String, Int] =
    map1.foldLeft(Map[String, Int]())((acc, ele) => acc + ele)

}