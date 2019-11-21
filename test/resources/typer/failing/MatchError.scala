object MatchError {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  Cons(3, Cons(4, Cons(5, Nil()))) match {
    case Cons(i, Nil()) => i 
    case Cons(i, Cons(j, l)) => i+j
    case Nil() => 0
    case Cons(0, Nil()) => 0 + 4
  }
}
