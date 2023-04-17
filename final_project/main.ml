#use "barrel.ml";;

let main = TestSuite.fromTests TestCaseTest.tests;;
let result = TestSuite.getResult @@ TestSuite.run main TestResult.init;;
let summary = TestResult.getSummary result;;
print_endline summary;;
