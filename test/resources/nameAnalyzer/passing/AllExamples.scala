object Arithmetic {
  def pow(b: Int, e: Int): Int = {
    if (e == 0) { 1 }
    else {
      if (e % 2 == 0) {
        val rec: Int = pow(b, e/2);
        rec * rec
      } else {
        b * pow(b, e - 1)
      }
    }
  }

  def gcd(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) {
      a + b
    } else {
      if (a < b) {
        gcd(a, b % a)
      } else {
        gcd(a % b, b)
      }
    }
  }

  Std.printInt(pow(0, 10));
  Std.printInt(pow(1, 5));
  Std.printInt(pow(2, 10));
  Std.printInt(pow(3, 3));
  Std.printInt(gcd(0, 10));
  Std.printInt(gcd(17, 99)); // 1
  Std.printInt(gcd(16, 46)); // 2
  Std.printInt(gcd(222, 888)) // 222
}

object BinaryOperations{
  Std.printBoolean(3 + 2 == 5);
  Std.printBoolean(2 - 3 == -1);
  Std.printBoolean(3 * 2 == 6);
  Std.printBoolean(5 / 2 == 2);
  Std.printBoolean(5 % 2 == 1);
  Std.printBoolean(5 < 6 && !(5 < 5) && !(6 < 5));
  Std.printBoolean(5 <= 5 && 5 <= 6 && ! (6 <= 5));
  Std.printString("tr" ++ "ue")
}

object Factorial {
  def fact(i: Int): Int = {
    if (i < 2) { 1 }
    else { 
      val rec: Int = fact(i-1);
      i * rec
    }
  }

  Std.printString("5! = "  ++ Std.intToString(fact(5)));
  Std.printString("10! = " ++ Std.intToString(fact(10)))
}

object Hanoi {
  def solve(n : Int) : Int = {
    if (n < 1) { 
      error("can't solve Hanoi for less than 1 plate")
    } else {
      if (n == 1) {
        1
      } else {
        2 * solve(n - 1) + 1
      }
    }
  }

  Std.printString("Hanoi for 4 plates: " ++ Std.intToString(solve(4)))
}

object Hello {
  Std.printString("Hello " ++ "world!")
}

object HelloInt {
  Std.printString("What is your name?");
  val name: String = Std.readString();
  Std.printString("Hello " ++ name ++ "! And how old are you?");
  val age: Int = Std.readInt();
  Std.printString(Std.intToString(age) ++ " years old then.")
}

object DivisionFail{
  def example(x: Int, y: Int) : Int = {2}
  
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List
  abstract class List

  val x : Int = 10;
  val y : Int = 0;
  x/y
}

object L {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List
 
  def isEmpty(l : List): Boolean = { l match {
    case Nil() => true
    case _ => false 
  }}

  def length(l: List): Int = { l match {
    case Nil() => 0
    case Cons(_, t) => 1 + length(t)
  }}

  def head(l: List): Int = {
    l match {
      case Cons(h, _) => h
      case Nil() => error("head(Nil)")
    }
  }

  def headOption(l: List): O.Option = {
    l match {
      case Cons(h, _) => O.Some(h)
      case Nil() => O.None()
    }
  }

  def reverse(l: List): List = {
    reverseAcc(l, Nil())
  }

  def reverseAcc(l: List, acc: List): List = {
    l match {
      case Nil() => acc
      case Cons(h, t) => reverseAcc(t, Cons(h, acc))
    }
  }

  def indexOf(l: List, i: Int): Int = {
    l match {
      case Nil() => -1
      case Cons(h, t) =>
        if (h == i) { 0 }
        else {
          val rec: Int = indexOf(t, i);
          if (0 <= rec) { rec + 1 }
          else { -1 }
        }
    }
  }

  def range(from: Int, to: Int): List = {
    if (to < from) { Nil() }
    else {
      Cons(from, range(from + 1, to))
    }
  }

  def sum(l: List): Int = { l match {
    case Nil() => 0
    case Cons(h, t) => h + sum(t)
  }}

  def concat(l1: List, l2: List): List = {
    l1 match {
      case Nil() => l2
      case Cons(h, t) => Cons(h, concat(t, l2))
    }
  }

  def contains(l: List, elem: Int): Boolean = { l match {
    case Nil() =>
      false
    case Cons(h, t) =>
      h == elem || contains(t, elem)
  }}

  abstract class LPair
  case class LP(l1: List, l2: List) extends LPair

  def merge(l1: List, l2: List): List = {
    l1 match {
      case Nil() => l2
      case Cons(h1, t1) =>
        l2 match {
          case Nil() => l1
          case Cons(h2, t2) =>
            if (h1 <= h2) {
              Cons(h1, merge(t1, l2))
            } else {
              Cons(h2, merge(l1, t2))
            }
        }
    }
  }

  def split(l: List): LPair = {
    l match {
      case Cons(h1, Cons(h2, t)) =>
        val rec: LPair = split(t);
        rec match {
          case LP(rec1, rec2) =>
            LP(Cons(h1, rec1), Cons(h2, rec2))
        }
      case _ =>
        LP(l, Nil())
    }
  }
  def mergeSort(l: List): List = {
    l match {
      case Nil() => l
      case Cons(h, Nil()) => l
      case l =>
        split(l) match {
          case LP(l1, l2) =>
            merge(mergeSort(l1), mergeSort(l2))
        }
    }
  }
  
  def toString(l: List): String = { l match {
    case Nil() => "List()"
    case more => "List(" ++ toString1(more) ++ ")"
  }}

  def toString1(l : List): String = { l match {
    case Cons(h, Nil()) => Std.intToString(h)
    case Cons(h, t) => Std.intToString(h) ++ ", " ++ toString1(t)
  }}

  def take(l: List, n: Int): List = {
    if (n <= 0) { Nil() }
    else { 
      l match {
        case Nil() => Nil()
        case Cons(h, t) =>
          Cons(h, take(t, n-1))
      }
    }
  }
    
}


object Locality {
  def fun(x : Int) : Int = {
    x
  }

  val x : Int = 3;
  val y : Int = 4;

  Std.printBoolean(fun(y) == 4)
}

object O {
  abstract class Option
  case class None() extends Option
  case class Some(v: Int) extends Option

  def isDefined(o: Option): Boolean = {
    o match {
      case None() => false
      case _ => true
    }
  }

  def get(o: Option): Int = {
    o match {
      case Some(i) => i
      case None() => error("get(None)")
    }
  }

  def getOrElse(o: Option, i: Int): Int = {
    o match {
      case None() => i
      case Some(oo) => oo
    }
  }

  def orElse(o1: Option, o2: Option): Option = {
    o1 match {
      case Some(_) => o1
      case None() => o2
    }
  }

  def toList(o: Option): L.List = {
    o match {
      case Some(i) => L.Cons(i, L.Nil())
      case None() => L.Nil()
    }
  }
}

object Printing {
  Std.printInt(0); Std.printInt(-222); Std.printInt(42);
  Std.printBoolean(true); Std.printBoolean(false);
  Std.printString(Std.digitToString(0));
  Std.printString(Std.digitToString(5));
  Std.printString(Std.digitToString(9));
  Std.printString(Std.intToString(0));
  Std.printString(Std.intToString(-111));
  Std.printString(Std.intToString(22));
  Std.printString("Hello " ++ "world!");
  Std.printString("" ++ "")
}

/** This module contains basic functionality for Amy,
  * including stub implementations for some built-in functions
  * (implemented in WASM or JavaScript)
  */
  object Std {
    def printInt(i: Int): Unit = {
      error("") // Stub implementation
    }
    def printString(s: String): Unit = {
      error("") // Stub implementation
    }
    def printBoolean(b: Boolean): Unit = {
      printString(booleanToString(b))
    }
  
    def readString(): String = {
      error("") // Stub implementation
    }
  
    def readInt(): Int = {
      error("") // Stub implementation
    }
  
    def intToString(i: Int): String = {
      if (i < 0) {
        "-" ++ intToString(-i)
      } else {
        val rem: Int = i % 10;
        val div: Int = i / 10;
        if (div == 0) { digitToString(rem) }
        else { intToString(div) ++ digitToString(rem) }
      }
    }
    def digitToString(i: Int): String = {
      error("") // Stub implementation
    }
    def booleanToString(b: Boolean): String = {
      if (b) { "true" } else { "false" }
    }
  }

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
