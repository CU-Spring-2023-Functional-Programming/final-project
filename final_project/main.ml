#use "barrel.ml";;

let main = TestSuite.fromTests TestCaseTest.tests;;
let result = TestSuite.run TestSuite.getResult main TestResult.init;;
let summary = TestResult.getSummary result;;
print_endline summary;;
