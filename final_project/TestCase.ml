module type TEST_CASE = sig
  type stateType

  val setUp: stateType * TestResult.testResultType -> stateType * TestResult.testResultType
  val tearDown: stateType * TestResult.testResultType -> stateType * TestResult.testResultType
  val initState: stateType
end

module TestCase(T: TEST_CASE) = struct
  include TestUtilities

  let getResult = fun (state, testResult) -> testResult

  let recordStarted = fun (state, testResult) -> (state, TestResult.testStarted testResult)
  let recordFailed testName errorMessage = fun (state, testResults) ->
    let testResults = TestResult.testFailed testName errorMessage testResults in
    (state, testResults)
  let ( >>= ) m f = fun s ->
    let s' = m s in
    f () s'
  let run testName main testResult =
    let packagedProgram = begin
      let (let*) = ( >>= ) in
      let* _ = recordStarted in
      let* _ = fun s ->
        try
        	let s' = T.setUp s in
          try
            main s'
          with
            | Failure (message) -> recordFailed testName message s'
            | _ -> recordFailed testName "Unknown error" s'
        with
        	| Failure (message) -> recordFailed testName ("Setup Failure: "^message) s
          | _ -> recordFailed testName ("Setup Failure: Unknown error") s
      in
      T.tearDown
    end in let (state, testResult) = packagedProgram (T.initState, testResult) in
    (state, testResult)
end
