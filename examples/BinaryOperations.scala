object BinaryOperations{
  if(3 + 2 == 5)
  {
    Std.printString("Correct sum");
    if(2 - 3 == -1)
    {
      Std.printString("Correct difference");
      if(3 * 2 == 6)
      {
        Std.printString("Correct multiplication");
        if(5 / 2 == 2)
        {
          Std.printString("Correct division");
          if(5 % 2 == 1)
          {
            Std.printString("Correct module");
            if(5 < 6 && !(5 < 5))
            {
              Std.printString("Correct comparison (less)");
              if(5 <= 5 && 5 <= 6)
              {
                Std.printString("Correct comparison (less equal)")
              }
              else
              {
                Std.printString("Wrong")
              }
            }
            else
            {
              Std.printString("Wrong")
            }
          }
          else
          {
            Std.printString("Wrong");
          }
        }
        else
        {
          Std.printString("Wrong");
        }
      }
      else
      {
        Std.printString("Wrong")
      }
    }
    else
    {
      Std.printString("Wrong")
    }
  }
  else
  {
    Std.printString("Wrong")
  }
}