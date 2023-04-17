#use "barrel.ml";;

let main = TestSuite.fromTests [
  TestCaseTest.run "TestCaseTest.testTemplateMethod" TestCaseTest.getResult TestCaseTest.testTemplateMethod;
  TestCaseTest.run "TestCaseTest.testResult" TestCaseTest.getResult TestCaseTest.testResult;
  TestCaseTest.run "TestCaseTest.testFailedResultFormatting" TestCaseTest.getResult TestCaseTest.testFailedResultFormatting;
  TestCaseTest.run "TestCaseTest.testFailedResult" TestCaseTest.getResult TestCaseTest.testFailedResult;
  TestCaseTest.run "TestCaseTest.testSuite" TestCaseTest.getResult TestCaseTest.testSuite
];;
let result = TestSuite.run TestSuite.getResult main TestResult.init;;
let summary = TestResult.getSummary result;;
print_endline summary;;
