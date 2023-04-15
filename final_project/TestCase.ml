module type TEST_CASE = sig
  type stateType

  val setUp: unit -> stateType * TestResult.testResultType -> stateType * TestResult.testResultType * unit
  val tearDown: unit -> stateType * TestResult.testResultType -> stateType * TestResult.testResultType * unit
  val initState: stateType
end

module TestCase(T: TEST_CASE) = struct
  include TestUtilities

  let getResult () = fun (state, testResult) -> (state, testResult, testResult)
  let recordStarted () = fun (state, testResult) -> (state, TestResult.testStarted(testResult), ())
  let recordFailed () = fun (state, testResult) -> (state, TestResult.testFailed(testResult), ())
  let return v = fun (state, testResult) -> (state, testResult, v)
  let ( >>= ) m f = fun s ->
    let (s', r, v) = m s in
    f v (s', r)
  let run finalValueGetter main testResult =
    let packagedProgram = begin
      let (let*) = ( >>= ) in
      let* v = recordStarted() in
      let* v = T.setUp v in
      let* v = fun s ->
        try
        	main v s
        with _ -> recordFailed v s
      in
      let* v = T.tearDown v in
      finalValueGetter v
    end in let (state, testResult, v) = packagedProgram (T.initState, testResult) in v
end
