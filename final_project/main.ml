#use "barrel.ml";;

let main =
  let (let*) = TestSuite.( >>= ) in
  let* _ = TestSuite.add (TestCaseTest.run "TestCaseTest.testTemplateMethod" TestCaseTest.getResult TestCaseTest.testTemplateMethod) in
  let* _ = TestSuite.add (TestCaseTest.run "TestCaseTest.testResult" TestCaseTest.getResult TestCaseTest.testResult) in
  let* _ = TestSuite.add (TestCaseTest.run "TestCaseTest.testFailedResultFormatting" TestCaseTest.getResult TestCaseTest.testFailedResultFormatting) in
  let* _ = TestSuite.add (TestCaseTest.run "TestCaseTest.testFailedResult" TestCaseTest.getResult TestCaseTest.testFailedResult) in
  TestSuite.add (TestCaseTest.run "TestCaseTest.testSuite" TestCaseTest.getResult TestCaseTest.testSuite);;
let result = TestSuite.run TestSuite.getResult main TestResult.init;;
let summary = TestResult.getSummary result;;
print_endline summary;;
