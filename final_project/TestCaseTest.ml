module TestCaseTest = struct
  open TestUtilities

  let resultSummaryGetter =
    let (let*) = WasRun.( >>= ) in
    let* result = WasRun.getResult in
    WasRun.return @@ TestResult.getSummary result

  let testTemplateMethod = fun (state, testResult) ->
    let result = WasRun.run "WasRun.testMethod" WasRun.getLog WasRun.testMethod TestResult.init in
    let _ = asrt ("setUp testMethod tearDown " = result, "It should have run the correct methods. Methods called were: "^result) in
    (state, testResult, ())

  let testResult = fun (state, testResult) ->
    let result = WasRun.run "WasRun.testMethod" resultSummaryGetter WasRun.testMethod TestResult.init in
    let _ = asrt ("1 run, 0 failed" = result, "It should have returned one run. Returned summary was: "^result) in
    (state, testResult, ())

  let testFailedResult = fun (state, testResult) ->
    let result = WasRun.run "WasRun.testBrokenMethod" resultSummaryGetter WasRun.testBrokenMethod TestResult.init in
    let _ = asrt ("1 run, 1 failed"^"\n\n"^"WasRun.testBrokenMethod: Broken method" = result, "It should have returned a failed run. Returned summary was: "^result) in
    (state, testResult, ())

  let testFailedResultFormatting = fun (state, testResult) ->
    let arbitraryTestName = "ArbitraryTestName" in
    let arbitraryErrorMessage = "ArbitraryErrorMessage" in
    let r = TestResult.init in
    let r = TestResult.testStarted r in
    let r = TestResult.testFailed arbitraryTestName arbitraryErrorMessage r in
    let summary = TestResult.getSummary r in
    let _ = TestUtilities.asrt ("1 run, 1 failed"^"\n\n"^arbitraryTestName^": "^arbitraryErrorMessage = summary, "Returned summary was: "^summary) in
    (state, testResult, ())

  let testSuite = fun (state, testResult) ->
    let main = begin
      let (let*) = TestSuite.( >>= ) in
      let* _ = TestSuite.add (WasRun.run "WasRun.testMethod" WasRun.getResult WasRun.testMethod) in
      TestSuite.add (WasRun.run "WasRun.testBrokenMethod" WasRun.getResult WasRun.testBrokenMethod)
    end in
    let result = TestSuite.run TestSuite.getResult main TestResult.init in
    let summary = TestResult.getSummary result in
    let _ = asrt ("2 run, 1 failed"^"\n\n"^"WasRun.testBrokenMethod: Broken method" = summary, "It should have two runs and one failed run. Returned summary was: "^summary) in
    (state, testResult, ())

  include TestCase(struct
    type stateType = unit

    let initState = ()
    let setUp = fun (state, testResult) -> (state, testResult, ())
    let tearDown = fun (state, testResult) -> (state, testResult, ())
  end)
end
