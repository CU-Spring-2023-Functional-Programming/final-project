module type TEST_CASE = sig
  type stateType

  val setUp: stateType * TestResult.testResultType -> stateType * TestResult.testResultType * unit
  val tearDown: stateType * TestResult.testResultType -> stateType * TestResult.testResultType * unit
  val initState: stateType
end

module TestCase(T: TEST_CASE) = struct
  include TestUtilities

  let getResult = fun (state, testResult) -> (state, testResult, testResult)
  let recordStarted = fun (state, testResult) -> (state, TestResult.testStarted(testResult), ())
  let recordFailed = fun (state, testResult) -> (state, TestResult.testFailed(testResult), ())
  let return v = fun (state, testResult) -> (state, testResult, v)
  let ( >>= ) m f = fun s ->
    let (s', r, v) = m s in
    f v (s', r)
  let run finalValueGetter main testResult =
    let packagedProgram = begin
      let (let*) = ( >>= ) in
      let* _ = recordStarted in
      let* _ = T.setUp in
      let* v = fun s ->
        try
        	main s
        with _ -> recordFailed s
      in
      let* _ = T.tearDown in
      finalValueGetter
    end in let (state, testResult, v) = packagedProgram (T.initState, testResult) in v
end
