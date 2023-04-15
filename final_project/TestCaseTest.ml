#use "WasRun.ml";;
#use "TestSuite.ml";;
#use "utilities.ml";;

module TestCaseTest = struct
  let setUp () = fun (state, testResult) -> (state, testResult, ())
  let tearDown () = fun (state, testResult) -> (state, testResult, ())
  let initState = ()

  let resultSummaryGetter () =
    let (let*) = WasRun.( >>= ) in
    let* result = WasRun.getResult () in
    WasRun.return @@ TestResult.getSummary result

  let testTemplateMethod () = fun (state, testResult) ->
    let result = WasRun.run WasRun.getLog WasRun.testMethod TestResult.init in
    let _ = asrt ("setUp testMethod tearDown " = result, "It should have run the correct methods. Methods called were: "^result) in
    (state, testResult, ())

  let testResult () = fun (state, testResult) ->
    let result = WasRun.run resultSummaryGetter WasRun.testMethod TestResult.init in
    let _ = asrt ("1 run, 0 failed" = result, "It should have returned one run. Returned summary was: "^result) in
    (state, testResult, ())

  let testFailedResult () = fun (state, testResult) ->
    let result = WasRun.run resultSummaryGetter WasRun.testBrokenMethod TestResult.init in
    let _ = asrt ("1 run, 1 failed" = result, "It should have returned a failed run. Returned summary was: "^result) in
    (state, testResult, ())

  let testFailedResultFormatting () = fun (state, testResult) ->
    let r = TestResult.init in
    let r = TestResult.testStarted r in
    let r = TestResult.testFailed r in
    let summary = TestResult.getSummary r in
    let _ = asrt ("1 run, 1 failed" = summary, "It should have one run and one failed run. Returned summary was: "^summary) in
    (state, testResult, ())

  let testSuite () = fun (state, testResult) ->
    let main = begin
      let (let*) = TestSuite.( >>= ) in
      let* v = TestSuite.add (WasRun.run WasRun.getResult WasRun.testMethod) () in
      TestSuite.add (WasRun.run WasRun.getResult WasRun.testBrokenMethod) v
    end in
    let result = TestSuite.run TestSuite.getResult main TestResult.init in
    let summary = TestResult.getSummary result in
    let _ = asrt ("2 run, 1 failed" = summary, "It should have two runs and one failed run. Returned summary was: "^summary) in
    (state, testResult, ())

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
      let* v = setUp v in
      let* v = fun s ->
        try
        	main v s
        with _ -> recordFailed v s
      in
      let* v = tearDown v in
      finalValueGetter v
    end in let (state, testResult, v) = packagedProgram (initState, testResult) in v
end
