
case class Union2[T1, T2](v1: Option[T1], v2: Option[T2])
case class Union3[T1, T2, T3](v1: Option[T1], v2: Option[T2], v3: Option[T3])
case class Union4[T1, T2, T3, T4](v1: Option[T1], v2: Option[T2], v3: Option[T3], v4 : Option[T4])

@union
class Union2Type[T1, T2]

@union
class Union3Type[T1, T2, T3]

@union
class Union4Type[T1, T2, T3, T4]

object Test extends App {

  val u2 = new Union2[String, Int](Some(""), None)
  import Union2._

  val stringOrInt = new Union2Type[String, Int]
  import stringOrInt._

  def computeValue(x: Int): Union2[String, Int] = {
    if (x > 0) "0"
    else 1
  }


  println(computeValue(5))


  val u3 = new Union3[String, Int, Boolean](None, None, Some(false))
  import Union3._

  val stringOrIntOrBoolean = new Union3Type[String, Int, Boolean]
  import stringOrIntOrBoolean._

  def computeValue2(x: Int): Union3[String, Int, Boolean] = {
    if (x< 0) false
    else true
  }

  println(computeValue2(0))
  println(computeValue2(-2))



  }

