object Arithmetic_0 {

  def pow_0(b_0: Int, e_0: Int): Int = {
    (if((e_0 == 0)) {
      1
    } else {
      (if(((e_0 % 2) == 0)) {
        (
          val rec_0: Int =
            pow_0(b_0, (e_0 / 2));
          (rec_0 * rec_0)
        )
      } else {
        (b_0 * pow_0(b_0, (e_0 - 1)))
      })
    })
  }

  def gcd_0(a_0: Int, b_1: Int): Int = {
    (if(((a_0 == 0) || (b_1 == 0))) {
      (a_0 + b_1)
    } else {
      (if((a_0 < b_1)) {
        gcd_0(a_0, (b_1 % a_0))
      } else {
        gcd_0((a_0 % b_1), b_1)
      })
    })
  }

  printInt_0(pow_0(0, 10));
  printInt_0(pow_0(1, 5));
  printInt_0(pow_0(2, 10));
  printInt_0(pow_0(3, 3));
  printInt_0(gcd_0(0, 10));
  printInt_0(gcd_0(17, 99));
  printInt_0(gcd_0(16, 46));
  printInt_0(gcd_0(222, 888))
}


object BinaryOperations_0 {

  printBoolean_0(((3 + 2) == 5));
  printBoolean_0(((2 - 3) == -(1)));
  printBoolean_0(((3 * 2) == 6));
  printBoolean_0(((5 / 2) == 2));
  printBoolean_0(((5 % 2) == 1));
  printBoolean_0((((5 < 6) && !((5 < 5))) && !((6 < 5))));
  printBoolean_0((((5 <= 5) && (5 <= 6)) && !((6 <= 5))));
  printString_0(("tr" ++ "ue"))
}


object Factorial_0 {

  def fact_0(i_0: Int): Int = {
    (if((i_0 < 2)) {
      1
    } else {
      (
        val rec_1: Int =
          fact_0((i_0 - 1));
        (i_0 * rec_1)
      )
    })
  }

  printString_0(("5! = " ++ intToString_0(fact_0(5))));
  printString_0(("10! = " ++ intToString_0(fact_0(10))))
}


object Hanoi_0 {

  def solve_0(n_0: Int): Int = {
    (if((n_0 < 1)) {
      error("can't solve Hanoi for less than 1 plate")
    } else {
      (if((n_0 == 1)) {
        1
      } else {
        ((2 * solve_0((n_0 - 1))) + 1)
      })
    })
  }

  printString_0(("Hanoi for 4 plates: " ++ intToString_0(solve_0(4))))
}


object Hello_0 {

  printString_0(("Hello " ++ "world!"))
}


object HelloInt_0 {

  printString_0("What is your name?");
  val name_0: String =
    readString_0();
  printString_0((("Hello " ++ name_0) ++ "! And how old are you?"));
  val age_0: Int =
    readInt_0();
  printString_0((intToString_0(age_0) ++ " years old then."))
}


object DivisionFail_0 {

  def example_0(x_0: Int, y_0: Int): Int = {
    2
  }

  case class Nil_0() extends List_0

  case class Cons_0(v: Int, v: List_0) extends List_0

  abstract class List_0

  val x_1: Int =
    10;
  val y_1: Int =
    0;
  (x_1 / y_1)
}


object L_0 {

  abstract class List_1

  case class Nil_1() extends List_1

  case class Cons_1(v: Int, v: List_1) extends List_1

  def isEmpty_0(l_0: List_1): Boolean = {
    l_0 match {
      case Nil_1() =>
        true
      case _ =>
        false
    }
  }

  def length_0(l_1: List_1): Int = {
    l_1 match {
      case Nil_1() =>
        0
      case Cons_1(_, t_0) =>
        (1 + length_0(t_0))
    }
  }

  def head_0(l_2: List_1): Int = {
    l_2 match {
      case Cons_1(h_0, _) =>
        h_0
      case Nil_1() =>
        error("head(Nil)")
    }
  }

  def headOption_0(l_3: List_1): Option_0 = {
    l_3 match {
      case Cons_1(h_1, _) =>
        Some_0(h_1)
      case Nil_1() =>
        None_0()
    }
  }

  def reverse_0(l_4: List_1): List_1 = {
    reverseAcc_0(l_4, Nil_1())
  }

  def reverseAcc_0(l_5: List_1, acc_0: List_1): List_1 = {
    l_5 match {
      case Nil_1() =>
        acc_0
      case Cons_1(h_2, t_1) =>
        reverseAcc_0(t_1, Cons_1(h_2, acc_0))
    }
  }

  def indexOf_0(l_6: List_1, i_1: Int): Int = {
    l_6 match {
      case Nil_1() =>
        -(1)
      case Cons_1(h_3, t_2) =>
        (if((h_3 == i_1)) {
          0
        } else {
          (
            val rec_2: Int =
              indexOf_0(t_2, i_1);
            (if((0 <= rec_2)) {
              (rec_2 + 1)
            } else {
              -(1)
            })
          )
        })
    }
  }

  def range_0(from_0: Int, to_0: Int): List_1 = {
    (if((to_0 < from_0)) {
      Nil_1()
    } else {
      Cons_1(from_0, range_0((from_0 + 1), to_0))
    })
  }

  def sum_0(l_7: List_1): Int = {
    l_7 match {
      case Nil_1() =>
        0
      case Cons_1(h_4, t_3) =>
        (h_4 + sum_0(t_3))
    }
  }

  def concat_0(l1_0: List_1, l2_0: List_1): List_1 = {
    l1_0 match {
      case Nil_1() =>
        l2_0
      case Cons_1(h_5, t_4) =>
        Cons_1(h_5, concat_0(t_4, l2_0))
    }
  }

  def contains_0(l_8: List_1, elem_0: Int): Boolean = {
    l_8 match {
      case Nil_1() =>
        false
      case Cons_1(h_6, t_5) =>
        ((h_6 == elem_0) || contains_0(t_5, elem_0))
    }
  }

  abstract class LPair_0

  case class LP_0(v: List_1, v: List_1) extends LPair_0

  def merge_0(l1_1: List_1, l2_1: List_1): List_1 = {
    l1_1 match {
      case Nil_1() =>
        l2_1
      case Cons_1(h1_0, t1_0) =>
        l2_1 match {
          case Nil_1() =>
            l1_1
          case Cons_1(h2_0, t2_0) =>
            (if((h1_0 <= h2_0)) {
              Cons_1(h1_0, merge_0(t1_0, l2_1))
            } else {
              Cons_1(h2_0, merge_0(l1_1, t2_0))
            })
        }
    }
  }

  def split_0(l_9: List_1): LPair_0 = {
    l_9 match {
      case Cons_1(h1_1, Cons_1(h2_1, t_6)) =>
        (
          val rec_3: LPair_0 =
            split_0(t_6);
          rec_3 match {
            case LP_0(rec1_0, rec2_0) =>
              LP_0(Cons_1(h1_1, rec1_0), Cons_1(h2_1, rec2_0))
          }
        )
      case _ =>
        LP_0(l_9, Nil_1())
    }
  }

  def mergeSort_0(l_10: List_1): List_1 = {
    l_10 match {
      case Nil_1() =>
        l_10
      case Cons_1(h_7, Nil_1()) =>
        l_10
      case l_11 =>
        split_0(l_11) match {
          case LP_0(l1_2, l2_2) =>
            merge_0(mergeSort_0(l1_2), mergeSort_0(l2_2))
        }
    }
  }

  def toString_0(l_12: List_1): String = {
    l_12 match {
      case Nil_1() =>
        "List()"
      case more_0 =>
        (("List(" ++ toString1_0(more_0)) ++ ")")
    }
  }

  def toString1_0(l_13: List_1): String = {
    l_13 match {
      case Cons_1(h_8, Nil_1()) =>
        intToString_0(h_8)
      case Cons_1(h_9, t_7) =>
        ((intToString_0(h_9) ++ ", ") ++ toString1_0(t_7))
    }
  }

  def take_0(l_14: List_1, n_1: Int): List_1 = {
    (if((n_1 <= 0)) {
      Nil_1()
    } else {
      l_14 match {
        case Nil_1() =>
          Nil_1()
        case Cons_1(h_10, t_8) =>
          Cons_1(h_10, take_0(t_8, (n_1 - 1)))
      }
    })
  }
}


object Locality_0 {

  def fun_0(x_2: Int): Int = {
    x_2
  }

  val x_3: Int =
    3;
  val y_2: Int =
    4;
  printBoolean_0((fun_0(y_2) == 4))
}


object O_0 {

  abstract class Option_0

  case class None_0() extends Option_0

  case class Some_0(v: Int) extends Option_0

  def isDefined_0(o_0: Option_0): Boolean = {
    o_0 match {
      case None_0() =>
        false
      case _ =>
        true
    }
  }

  def get_0(o_1: Option_0): Int = {
    o_1 match {
      case Some_0(i_2) =>
        i_2
      case None_0() =>
        error("get(None)")
    }
  }

  def getOrElse_0(o_2: Option_0, i_3: Int): Int = {
    o_2 match {
      case None_0() =>
        i_3
      case Some_0(oo_0) =>
        oo_0
    }
  }

  def orElse_0(o1_0: Option_0, o2_0: Option_0): Option_0 = {
    o1_0 match {
      case Some_0(_) =>
        o1_0
      case None_0() =>
        o2_0
    }
  }

  def toList_0(o_3: Option_0): List_1 = {
    o_3 match {
      case Some_0(i_4) =>
        Cons_1(i_4, Nil_1())
      case None_0() =>
        Nil_1()
    }
  }
}


object Printing_0 {

  printInt_0(0);
  printInt_0(-(222));
  printInt_0(42);
  printBoolean_0(true);
  printBoolean_0(false);
  printString_0(digitToString_0(0));
  printString_0(digitToString_0(5));
  printString_0(digitToString_0(9));
  printString_0(intToString_0(0));
  printString_0(intToString_0(-(111)));
  printString_0(intToString_0(22));
  printString_0(("Hello " ++ "world!"));
  printString_0(("" ++ ""))
}


object Std_0 {

  def printInt_0(i_5: Int): Unit = {
    error("")
  }

  def printString_0(s_0: String): Unit = {
    error("")
  }

  def printBoolean_0(b_2: Boolean): Unit = {
    printString_0(booleanToString_0(b_2))
  }

  def readString_0(): String = {
    error("")
  }

  def readInt_0(): Int = {
    error("")
  }

  def intToString_0(i_6: Int): String = {
    (if((i_6 < 0)) {
      ("-" ++ intToString_0(-(i_6)))
    } else {
      (
        val rem_0: Int =
          (i_6 % 10);
        val div_0: Int =
          (i_6 / 10);
        (if((div_0 == 0)) {
          digitToString_0(rem_0)
        } else {
          (intToString_0(div_0) ++ digitToString_0(rem_0))
        })
      )
    })
  }

  def digitToString_0(i_7: Int): String = {
    error("")
  }

  def booleanToString_0(b_3: Boolean): String = {
    (if(b_3) {
      "true"
    } else {
      "false"
    })
  }
}


object TestLists_0 {

  val l_15: List_1 =
    Cons_1(5, Cons_1(-(5), Cons_1(-(1), Cons_1(0, Cons_1(10, Nil_1())))));
  printString_0(toString_0(concat_0(Cons_1(1, Cons_1(2, Nil_1())), Cons_1(3, Nil_1()))));
  printInt_0(sum_0(l_15));
  printString_0(toString_0(mergeSort_0(l_15)))
}