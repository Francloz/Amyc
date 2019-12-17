object MyTest {
  // def x(i : Int, j : Int) : Int = {i*j}
  // def y() : Int = {3}

  abstract class List
  case class Nil() extends List
  case class Cons(h: Boolean, t: List) extends List
  //val ys : List = Cons(3, Nil());
  //val xs : List = Cons(2, ys);
  val l : List = Cons(true, Nil()); //Nil();
  val correct : Boolean = l match {
    case Cons(x,_) => x
    case Nil() => false
  };
  //val correct : Boolean = true;
  if(correct){error("Yes")}else{error("No")}
}
