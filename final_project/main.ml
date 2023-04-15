#use "TestCaseTest.ml";;

let _ = TestCaseTest.run TestCaseTest.return TestCaseTest.testTemplateMethod
let _ = TestCaseTest.run TestCaseTest.return TestCaseTest.testResult
let _ = TestCaseTest.run TestCaseTest.return TestCaseTest.testFailedResultFormatting
let _ = TestCaseTest.run TestCaseTest.return TestCaseTest.testFailedResult
let _ = TestCaseTest.run TestCaseTest.return TestCaseTest.testSuite
