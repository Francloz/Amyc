package amyc.test

import amyc.parsing._
import org.junit.Test

class LexerTests extends TestSuite {
  val pipeline = Lexer andThen DisplayTokens

  val baseDir = "lexer"

  val outputExt = "txt"

  @Test def testKeywords = shouldOutput("Keywords")
  @Test def testMultComments = shouldOutput("MultilinedComment")
  @Test def testOneComments = shouldOutput("OneLinedComment")
  @Test def testComments = shouldOutput("Comments")

  @Test def testArithmetic = shouldOutput("Arithmetic")
  @Test def testBinaryOperations = shouldOutput("BinaryOperations")
  @Test def testFactorial = shouldOutput("Factorial")
  @Test def testHello = shouldOutput("Hello")
  @Test def testHelloInt = shouldOutput("HelloInt")
  @Test def testInvalidOperations = shouldOutput("InvalidOperations")
  @Test def testLocality = shouldOutput("Locality")
  @Test def testPrinting = shouldOutput("Printing")
  @Test def testTestLists = shouldOutput("TestLists")

  @Test def testNestedComments = shouldFail("NestedComments")
  @Test def testUnclosedComment = shouldFail("UnclosedComment")
  @Test def testSingleAmp = shouldFail("SingleAmp")

}
