import org.scalatest.funsuite.AnyFunSuite

class chapter3test extends AnyFunSuite {

    val l: ListA[Int] = ListA(1,2,3,4)

    test("Tail function should return every element in listA except first"){
      assert(ListA.tail(l) == ListA(2,3,4))
    }

  test("Set Head should change head of list to passed element"){
    val nHead: ListA[Int] = ListA.setHead(l, 1000)
    assert(ListA.headOption(nHead) == Some(1000))
  }

  test("drop first n elements from list"){
    assert(ListA.drop(l, 3) == ListA(4))
  }

  test("drop elements as long as condition is true"){
    assert(ListA.dropWhileCurry(l)(_<3) == ListA(3,4))
  }

  test("product using foldR should give correct result"){
    val la: ListA[Double] = ListA(2.0, 10.0, 20.0)
    assert(ListA.productR(la) == 400 )
  }

  test("length using foldR should give correct result"){
    assert(ListA.lengthR(l) == 4 )
  }

  test("product using foldLeft should give correct result"){
    val la: ListA[Double] = ListA(2.0, 10.0, 20.0)
    assert(ListA.product(la) == 400 )
  }

  test("sum using foldLeft should give correct result"){
    val la: ListA[Double] = ListA(2.0, 10.0, 20.0)
    assert(ListA.sum(la) == 32 )
  }

  test("length using foldLeft should give correct result"){
    assert(ListA.length(l) == 4 )
  }

  test ("reverse should reverse list"){
    assert(ListA.reverse(l) == ListA(4,3,2,1))
  }

  test("append should add element to end of list"){
    assert(ListA.append(l,600) ==ListA(1,2,3,4,600))
  }

  test("increment should add one to list"){
    assert(ListA.increment(l) == ListA(2,3,4,5))
  }

  test("doubleToString  should make every element of list a string"){
    val la: ListA[Double] = ListA(2.0, 10.0, 20.0)
    val ld : ListA[String] = ListA.doubleToString(la)
    ListA.forEach(ld)(h => assert(h.isInstanceOf[String]))
  }

  test("Map function tests"){
    assert(ListA.map(l)(h => if(h % 2==0) h *5 else h) == ListA(1,10,3,20))
  }


}