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