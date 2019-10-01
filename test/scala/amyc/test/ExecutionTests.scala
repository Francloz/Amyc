package amyc.test

import org.junit.Test

abstract class ExecutionTests extends TestSuite {

  val baseDir = "interpreter"

  val outputExt = "txt"

  @Test def testEmptyObject = shouldOutput("EmptyObject")
  
  @Test def testConditionalBooleans = shouldFail("ConditionalBooleans")

  @Test def testDivisionBy0 = shouldFail("Division")

  @Test def testMinimalError = shouldFail("MinimalError")

}
