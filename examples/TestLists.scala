object TestLists {
  // New
   val l_aux: L.List = L.Cons(5, L.Cons(1 ,L.Cons(1 ,L.Nil())));

   Std.printInt(L.sum(L.concat(l_aux,L.Cons(5, L.Nil()))));
   Std.printString((L.Cons(5, L.Nil()) match {
    case L.Cons(h, L.Nil()) => Std.intToString(h)
    case L.Cons(h, t) => "Shouldnt have entered"
   }));

  Std.printString(L.toString(L.Cons(5, L.Nil())))

  //  //Old
  // val l: L.List = L.Cons(5, L.Cons(-5, L.Cons(-1, L.Cons(0, L.Cons(10, L.Nil())))));
  // Std.printString(L.toString(L.concat(L.Cons(1, L.Cons(2, L.Nil())), L.Cons(3, L.Nil()))));
  // Std.printInt(L.sum(l));
  // Std.printString(L.toString(L.mergeSort(l)))
}
