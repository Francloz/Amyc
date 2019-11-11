object DivisionFail{
  def example(x: Int, y: Int) : Int = {2}
  
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List
  abstract class List

  val x : Int = 10;
  val y : Int = 0;
  x/y
}