object TestLists {
  // val str : String = "ex1";
  // val str2 : String = str;
  // if(str2 == str)
  //   {error("")}
  // else
  //   {Std.printString("Correct")}

  //  //Old
  val l: L.List = L.Cons(5, L.Cons(-5, L.Cons(-1, L.Cons(0, L.Cons(10, L.Nil())))));
  Std.printString(L.toString(L.concat(L.Cons(1, L.Cons(2, L.Nil())), L.Cons(3, L.Nil()))));
  Std.printInt(L.sum(l));
  Std.printString(L.toString(L.mergeSort(l)))
}
