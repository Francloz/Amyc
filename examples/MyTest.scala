object ParamAndLocal {
  def Foo(i: Int): Int = {
    val i: Int = i + 1;
    (val i: Int = 2; 
    32 + i)
  }
  
  abstract class List
  case class nil() extends List
  case class Cons(h: Int, t: List) extends List

  Cons(3, nil()) match {
    case Cons(h1, Cons(h2, t)) => 2
    case nil() => Foo(3)
  }
}
