object Arithmetic {

  def pow(b: Int, e: Int): Int = {
    (if((e == 0)) {
      1
    } else {
      (if(((e % 2) == 0)) {
        (
          val rec: Int =
            pow(b, (e / 2));
          (rec * rec)
        )
      } else {
        (b * pow(b, (e - 1)))
      })
    })
  }

  def gcd(a: Int, b: Int): Int = {
    (if(((a == 0) || (b == 0))) {
      (a + b)
    } else {
      (if((a < b)) {
        gcd(a, (b % a))
      } else {
        gcd((a % b), b)
      })
    })
  }

  Std.printInt(pow(0, 10));
  Std.printInt(pow(1, 5));
  Std.printInt(pow(2, 10));
  Std.printInt(pow(3, 3));
  Std.printInt(gcd(0, 10));
  Std.printInt(gcd(17, 99));
  Std.printInt(gcd(16, 46));
  Std.printInt(gcd(222, 888))
}


object BinaryOperations {

  Std.printBoolean(((3 + 2) == 5));
  Std.printBoolean(((2 - 3) == -(1)));
  Std.printBoolean(((3 * 2) == 6));
  Std.printBoolean(((5 / 2) == 2));
  Std.printBoolean(((5 % 2) == 1));
  Std.printBoolean((((5 < 6) && !((5 < 5))) && !((6 < 5))));
  Std.printBoolean((((5 <= 5) && (5 <= 6)) && !((6 <= 5))));
  Std.printString(("tr" ++ "ue"))
}


object Factorial {

  def fact(i: Int): Int = {
    (if((i < 2)) {
      1
    } else {
      (
        val rec: Int =
          fact((i - 1));
        (i * rec)
      )
    })
  }

  Std.printString(("5! = " ++ Std.intToString(fact(5))));
  Std.printString(("10! = " ++ Std.intToString(fact(10))))
}


object Hanoi {

  def solve(n: Int): Int = {
    (if((n < 1)) {
      error("can't solve Hanoi for less than 1 plate")
    } else {
      (if((n == 1)) {
        1
      } else {
        ((2 * solve((n - 1))) + 1)
      })
    })
  }

  Std.printString(("Hanoi for 4 plates: " ++ Std.intToString(solve(4))))
}


object Hello {

  Std.printString(("Hello " ++ "world!"))
}


object HelloInt {

  Std.printString("What is your name?");
  val name: String =
    Std.readString();
  Std.printString((("Hello " ++ name) ++ "! And how old are you?"));
  val age: Int =
    Std.readInt();
  Std.printString((Std.intToString(age) ++ " years old then."))
}


object DivisionFail {

  val x: Int =
    10;
  val y: Int =
    0;
  (x / y)
}


object Locality {

  def fun(x: Int): Int = {
    x
  }

  val x: Int =
    3;
  val y: Int =
    4;
  Std.printBoolean((fun(y) == 4))
}


object Printing {

  Std.printInt(0);
  Std.printInt(-(222));
  Std.printInt(42);
  Std.printBoolean(true);
  Std.printBoolean(false);
  Std.printString(Std.digitToString(0));
  Std.printString(Std.digitToString(5));
  Std.printString(Std.digitToString(9));
  Std.printString(Std.intToString(0));
  Std.printString(Std.intToString(-(111)));
  Std.printString(Std.intToString(22));
  Std.printString(("Hello " ++ "world!"));
  Std.printString(("" ++ ""))
}


object TestLists {

  val l: L.List =
    L.Cons(5, L.Cons(-(5), L.Cons(-(1), L.Cons(0, L.Cons(10, L.Nil())))));
  Std.printString(L.toString(L.concat(L.Cons(1, L.Cons(2, L.Nil())), L.Cons(3, L.Nil()))));
  Std.printInt(L.sum(l));
  Std.printString(L.toString(L.mergeSort(l)))
}
