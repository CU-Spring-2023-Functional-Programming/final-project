#use "TestUtilities.ml";;
#use "TestResult.ml";;
#use "TestSuite.ml";;
#use "TestCase.ml";;
#use "WasRun.ml";;
#use "TestCaseTest.ml";;

TestCaseTest.run TestCaseTest.getResult TestCaseTest.testTemplateMethod TestResult.init;;

let main =
  let (let*) = TestSuite.( >>= ) in
  let* _ = TestSuite.add (TestCaseTest.run TestCaseTest.getResult TestCaseTest.testTemplateMethod) in
  let* _ = TestSuite.add (TestCaseTest.run TestCaseTest.getResult TestCaseTest.testResult) in
  let* _ = TestSuite.add (TestCaseTest.run TestCaseTest.getResult TestCaseTest.testFailedResultFormatting) in
  let* _ = TestSuite.add (TestCaseTest.run TestCaseTest.getResult TestCaseTest.testFailedResult) in
  TestSuite.add (TestCaseTest.run TestCaseTest.getResult TestCaseTest.testSuite);;
let result = TestSuite.run TestSuite.getResult main TestResult.init;;
let summary = TestResult.getSummary result;;
TestUtilities.asrt ("5 run, 0 failed" = summary, "It should have runs and no failed runs. Returned summary was: "^summary);;
