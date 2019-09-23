package amyc.test

import org.junit.Test

abstract class ExecutionTests extends TestSuite {

  val baseDir = "interpreter"

  val outputExt = "txt"

  @Test def testEmptyObject = shouldOutput("EmptyObject")

  @Test def testBinaryOps = shouldOutput("BinaryOps")

  @Test def testMinimalError = shouldFail("MinimalError")

}
