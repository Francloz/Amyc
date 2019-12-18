object MyTest {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List
  
  Nil() match {
    case Cons(1, Cons(2, Cons(1, Nil()))) => 11
    case Cons(1, Cons(2, Cons(1, Cons(1, Cons(2, Cons(1, Cons(1, Cons(2, Cons(1, Nil()))))))))) => 22
  }
}
