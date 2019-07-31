package exercise
import org.scalatest.FunSuite

class Assigment1Test extends FunSuite {
  val assigment = new Assigment1

  test("show method") {
    val result = assigment.show(2,2)
    val expectedResult = List(1,1,2,2)
    assert(result.equals(expectedResult))
  }

  test("reverse method"){
    val list = List(1,2,3,4)
    val result = assigment.reverse(list)
    val expectedResult = List(4,3,2,1)
    assert(result.equals(expectedResult))
  }

  test("rotate method"){
    val array = Array(1,2,3,4)
    val result = assigment.rotate(array,2)
    val expectedResult = Array(3,4,1,2)
    assert(result.deep == expectedResult.deep)
  }

  test("fill method"){
    val result = assigment.fill(1)(4)
    val expectedResult = List(1,1,1,1)
    assert(result.equals(expectedResult))
  }

  test("dedupe method"){
    val list = List(1,1,3,4)
      val result = assigment.dedupe(list)
      val expectedResult = List(1,3,4)
      assert(result.equals(expectedResult))
  }

  test("word count method"){
    val sentence = "hi hi how you"
    val result = assigment.wordCount(sentence)
    val expectedResult = Map(("hi",2),("how",1),("you",1))
    assert(result.equals(expectedResult))
  }

  test("remove tuple whose values are odd"){
    val map = Map(("hi",2),("how",1),("you",1))
    val result = assigment.removeOdd(map)
    val expectedResult = Map(("hi",2))
    assert(result.equals(expectedResult))
  }

  test("remove tuple whose keys are passed as list"){
    val listOfKeysShouldRemoved = List("how")
    val map = Map(("hi",2),("how",1),("you",1))
    val result = assigment.removeKeys(listOfKeysShouldRemoved,map)
    val expectedResult = Map(("hi",2),("you",1))
    assert(result.equals(expectedResult))
  }
}
