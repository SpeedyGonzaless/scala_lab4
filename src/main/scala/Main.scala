import scala.collection.mutable.ListBuffer

object Main extends App{
  val strings = new Seq("f", "d", "a")
  val strings2 = new Seq("c", "b")
  val strings3 = strings2.concat(strings)
  println(strings3)


  val numbers = new Seq(1,5,4,2)
  numbers.multy(5)
  numbers.multy(2)
  val numbers2 = new Seq(3, 9)
  numbers2.multy(2)
  numbers2.multy(3)
  val numbers3 = numbers.concat(numbers2)
  println(numbers3)

  val floatNumbers1 = new Seq(1.3, 2.2, 10.11111)
  floatNumbers1.multy(5)
  val floatNumbers2 = new Seq(81.66666666, 19.9)
  val floatNumbers3 = floatNumbers2.concat(floatNumbers1)
  println(floatNumbers3)
}


class Seq[T : Ordering] (initialElems: T*)(implicit toStr: ToStr[T]) {

  override def toString: String = {
    val t = toStr(fac(factors), elems: _*)
    t.mkString(" ")
  }

  val elems = initialElems.sorted

  def concat(a:Seq[T]) = {
    val t1 = toStr(fac(factors), this.elems: _*).toList
    val t2 = toStr(fac(a.factors), a.elems: _*).toList
    new Seq[T](t1 ++ t2:_ *)
  }

  val factors = ListBuffer[Int](1)

  def fac(xs: ListBuffer[Int]): Int = {
    xs.foldLeft(1)((m,n) => m*n)
  }

  def multy(a: Int)(implicit ev: Numeric[T]): Unit = {
    factors += a
  }
}

trait ToStr[T] {
  def apply(fac: Int, elems: T*): ListBuffer[T]
}
object ToStr {
  implicit val int: ToStr[Int] =
    (fac, elems) => {
      val list = ListBuffer[Int]()
      elems.foreach((i: Int) => {
        list += fac * i
      })
      list
    }
    implicit val double: ToStr[Double] =
      (fac, elems) => {
        val list = ListBuffer[Double]()
        elems.foreach((i: Double) => {
          list += fac * i
        })
        list
      }
    implicit val float: ToStr[Float] =
      (fac, elems) => {
        val list = ListBuffer[Float]()
        elems.foreach((i: Float) => {
          list += fac * i
        })
        list
      }
    implicit val long: ToStr[Long] =
      (fac, elems) => {
        val list = ListBuffer[Long]()
        elems.foreach((i: Long) => {
          list += fac * i
        })
        list
      }
  implicit val str: ToStr[String] = (_, elems) => {
    val list = ListBuffer[String]()
    elems.foreach((i: String) => {
      list += i
    })
    list
  }
}