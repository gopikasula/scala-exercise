package exercise

class Assigment1 {

  def show(n:Int,numberOfRepeat:Int):List[Int] = {
    (1 to n).toList map{ index => List.fill(numberOfRepeat)(index) } flatten
  }

  def reverse(list:List[Int]):List[Int]={
    list.foldLeft(List[Int]()) { (acc,ele) => ele +: acc}
  }

  def rotate(arr:Array[Int],r:Int):Array[Int] = {
    def rotateOnce(array:Array[Int],n:Int):Array[Int] = {
      if(n > r)
        array
      else
        rotateOnce(array.tail :+ array.head, n + 1 )
    }
    rotateOnce(arr,1)
  }

  def fill(e:Int)(n:Int) = { List.range(0,n) map { n => e }}

  def dedupe(list:List[Int]):List[Int] = {
    list.foldLeft(List[Int]()) { (acc,ele) => if(acc.contains(ele)) acc else acc :+ ele }
  }

  def deDupeByRecursion(list:List[Int]):List[Int] = {
    def deDupe(acc:List[Int],list:List[Int]):List[Int] = {
      if(list.isEmpty)
        acc
      else
      if (acc.contains(list.head))
        deDupe(acc,list.tail)
      else
        deDupe(acc :+ list.head,list.tail)
    }
    deDupe(List[Int](),list)
  }

  def wordCount(sentence:String):Map[String,Int]= {
    sentence.split(" ") groupBy{x=>x} map {x =>(x._1,x._2.length)}
  }

  // Remove all keys from Map if their value are odd
 def removeOdd(map:Map[String, Int]):Map[String, Int]={
   map.foldLeft(Map[String,Int]()){ (acc,ele)=> { if (ele._2 % 2 == 0 ) acc + ele else acc }}
  }

  def removeKeys(keys:List[String],map:Map[String,Int]):Map[String,Int]={
    map.filter{case(key,value) => !keys.contains(key) }
  }
}
