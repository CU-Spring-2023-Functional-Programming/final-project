module type TEST_CASE = sig
  type stateType

  val setUp: stateType * TestResult.testResultType -> stateType * TestResult.testResultType * unit
  val tearDown: stateType * TestResult.testResultType -> stateType * TestResult.testResultType * unit
  val initState: stateType
end

module TestCase(T: TEST_CASE) = struct
  include TestUtilities

  let getResult = fun (state, testResult) -> testResult

  let recordStarted = fun (state, testResult) -> (state, TestResult.testStarted testResult, ())
  let recordFailed testName errorMessage = fun (state, testResults) ->
    let testResults = TestResult.testFailed testName errorMessage testResults in
    (state, testResults, ())
  let return v = fun (state, testResult) -> (state, testResult, v)
  let ( >>= ) m f = fun s ->
    let (s', r, v) = m s in
    f v (s', r)
  let run testName main testResult =
    let packagedProgram = begin
      let (let*) = ( >>= ) in
      let* _ = recordStarted in
      let* _ = T.setUp in
      let* v = fun s ->
        try
        	main s
        with
          | Failure (message) -> recordFailed testName message s
          | _ -> recordFailed testName "Unknown error" s
      in
      T.tearDown
    end in let (state, testResult, v) = packagedProgram (T.initState, testResult) in
    (state, testResult)
end
