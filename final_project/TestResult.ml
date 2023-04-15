module type TEST_RESULT = sig
  type testResultType

  val init: testResultType
  val getSummary: testResultType -> string
  val testStarted: testResultType -> testResultType
  val testFailed: string -> string -> testResultType -> testResultType
end

module TestResult: TEST_RESULT = struct
  type testResultType = int * (string * string) list

  let init = (0, [])
  let getSummary (runCount, failures) =
    let formatErrorMessage current (testName, errorMessage) = current^"\n\n"^testName^": "^errorMessage in
    let errorMessages = List.fold_left formatErrorMessage "" failures in
    (string_of_int runCount)^" run, "^(string_of_int @@ List.length failures)^" failed"^errorMessages
  let testStarted (runCount, failures) = (runCount + 1, failures)
  let testFailed testName errorMessage (runCount, failures) = (runCount, (testName, errorMessage)::failures)
end
