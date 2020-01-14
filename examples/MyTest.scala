object MyTest {
  
  def foo() : Int = {3 + 3 + 1}

  val y : Int = 5;
  var x : Int = 6;
  val z : String = "";
  val w : String = z;
  x=(x+1) + x=(x+1);
  if (x+foo() < 6) { x=(x+1) } else {y+1};
  x=(x+1);
  x=3;
  x=(x+1);
  x match{
    case 1 => 3 + 5 + x
    case 12 => 4 + x + y
    case 5 => foo()
    case _ => foo() + 3 + 3
  };
  w == z
}
