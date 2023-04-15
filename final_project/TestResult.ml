module type TEST_RESULT = sig
  type testResultType

  val init: testResultType
  val getSummary: testResultType -> string
  val testStarted: testResultType -> testResultType
  val testFailed: testResultType -> testResultType
end

module TestResult: TEST_RESULT = struct
  type testResultType = int * int

  let init = (0, 0)
  let getSummary (runCount, errorCount) = (string_of_int runCount)^" run, "^(string_of_int errorCount)^" failed"
  let testStarted (runCount, errorCount) = (runCount + 1, errorCount)
  let testFailed (runCount, errorCount) = (runCount, errorCount + 1)
end
