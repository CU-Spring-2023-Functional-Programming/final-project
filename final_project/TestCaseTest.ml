module TestCaseTest = struct
  open TestUtilities

  let resultSummaryGetter getResult finalInfo = TestResult.getSummary @@ getResult finalInfo

  let testTemplateMethod = fun (state, testResult) ->
    let result = WasRun.getLog @@ WasRun.run "WasRun.testMethod" WasRun.testMethod TestResult.init in
    let _ = asrt ("setUp testMethod tearDown " = result, "It should have run the correct methods. Methods called were: "^result) in
    (state, testResult)

  let testResult = fun (state, testResult) ->
    let summary = resultSummaryGetter WasRun.getResult @@ WasRun.run "WasRun.testMethod" WasRun.testMethod TestResult.init in
    let _ = asrt ("1 run, 0 failed" = summary, "It should have returned one run. Returned summary was: "^summary) in
    (state, testResult)

  let testFailedResult = fun (state, testResult) ->
    let result = resultSummaryGetter WasRun.getResult @@ WasRun.run "WasRun.testBrokenMethod" WasRun.testBrokenMethod TestResult.init in
    let _ = asrt ("1 run, 1 failed"^"\n\n"^"WasRun.testBrokenMethod: Broken method" = result, "It should have returned a failed run. Returned summary was: "^result) in
    (state, testResult)

  let testTeardownCalledAfterFailedResult = fun (state, testResult) ->
    let result = WasRun.getLog @@ WasRun.run "TestCaseTest -> WasRun.testBrokenMethod" WasRun.testBrokenMethod TestResult.init in
    let _ = asrt ("setUp tearDown " = result, "It should have called setUp and tearDown. Returned summary was: "^result) in
    (state, testResult)

  let testFailedSetup = fun (state, testResult) ->
    let result = resultSummaryGetter FailedSetup.getResult @@ FailedSetup.run "TestCaseTest -> FailedSetup.testMethod" FailedSetup.testMethod TestResult.init in
    let _ = asrt ("1 run, 1 failed"^"\n\n"^"TestCaseTest -> FailedSetup.testMethod: Setup Failure: Broken setup" = result, "It should have returned a failed run. Returned summary was: "^result) in
    (state, testResult)

  let testTeardownCalledAfterFailedSetup = fun (state, testResult) ->
    let result = FailedSetup.getLog @@ FailedSetup.run "TestCaseTest -> FailedSetup.testBrokenMethod" FailedSetup.testMethod TestResult.init in
    let _ = asrt ("tearDown " = result, "It should have called tearDown. Returned summary was: "^result) in
    (state, testResult)

  let testFailedResultFormatting = fun (state, testResult) ->
    let arbitraryTestName = "ArbitraryTestName" in
    let arbitraryErrorMessage = "ArbitraryErrorMessage" in
    let r = TestResult.init in
    let r = TestResult.testStarted r in
    let r = TestResult.testFailed arbitraryTestName arbitraryErrorMessage r in
    let summary = TestResult.getSummary r in
    let _ = TestUtilities.asrt ("1 run, 1 failed"^"\n\n"^arbitraryTestName^": "^arbitraryErrorMessage = summary, "Returned summary was: "^summary) in
    (state, testResult)

  let assertTestSuiteSummary main =
    let result = TestSuite.getResult @@ TestSuite.run main TestResult.init in
      let summary = TestResult.getSummary result in
      let _ = asrt ("2 run, 1 failed"^"\n\n"^"WasRun.testBrokenMethod: Broken method" = summary, "It should have two runs and one failed run. Returned summary was: "^summary) in
      ()

  let testSuite = fun (state, testResult) ->
    let main = begin
      let ( let* ) = TestSuite.( >>= ) in
      let* _ = TestSuite.add (WasRun.run "WasRun.testMethod" WasRun.testMethod) in
      TestSuite.add (WasRun.run "WasRun.testBrokenMethod" WasRun.testBrokenMethod)
    end in
    let _ = assertTestSuiteSummary main in
    (state, testResult)

  let testSuiteFromTestCase = fun (state, testResult) ->
    let main = TestSuite.fromTests WasRun.tests in
    let _ = assertTestSuiteSummary main in
    (state, testResult)

  include TestCase(struct
    type stateType = unit

    let initState = ()
    let setUp = fun (state, testResult) -> (state, testResult)
    let tearDown = fun (state, testResult) -> (state, testResult)
  end)

  let tests = [
    run "TestCaseTest.testTemplateMethod" testTemplateMethod;
    run "TestCaseTest.testResult" testResult;
    run "TestCaseTest.testFailedResultFormatting" testFailedResultFormatting;
    run "TestCaseTest.testFailedResult" testFailedResult;
    run "TestCaseTest.testTeardownCalledAfterFailedResult" testTeardownCalledAfterFailedResult;
    run "TestCaseTest.testFailedSetup" testFailedSetup;
    run "TestCaseTest.testTeardownCalledAfterFailedSetup" testTeardownCalledAfterFailedSetup;
    run "TestCaseTest.testSuite" testSuite;
    run "TestCaseTest.testSuiteFromTestCase" testSuiteFromTestCase;
  ]
end
