object Locality {
  def fun(x : Int) : Int = {
    x
  }

  val x : Int = 3;
  val y : Int = 4;

  Std.printBoolean(fun(y) == 4)
}