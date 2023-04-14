let asrt = function
 | (true,_) -> ()
 | (false, str) -> failwith ("Assertion failure: "^str)


module Result = struct
  let init = (0, 0)

  let getSummary (runCount, failedCount) = (string_of_int runCount)^" run, 0 failed"

  let incrementRunCount (runCount, failedCount) = (runCount + 1, failedCount)
end


module WasRun = struct
  let setUp () = fun (wasRun, log, result) -> ((wasRun, log^"setUp ", result), ())
  let tearDown () = fun (wasRun, log, result) -> ((wasRun, log^"tearDown ", result), ())
  let init = (false, "", Result.init)

  let getWasRun () = fun (wasRun, log, result) -> ((wasRun, log, result), wasRun)
  let getLog () = fun (wasRun, log, result) -> ((wasRun, log, result), log)
  let getResult () = fun (wasRun, log, result) -> ((wasRun, log, result), result)

  let recordStarted () = fun (wasRun, log, result) -> ((wasRun, log, Result.incrementRunCount(result)), ())

  let testMethod () = fun (_, log, result) -> ((true, log^"testMethod ", result), ())

  let return r = fun state -> (state, r)
  let ( >>= ) m f = fun s ->
      let (s', v) = m s in
      f v s'
  let run finalValueGetter main =
    let packagedProgram = begin
      let (let*) = ( >>= ) in
      let* r = recordStarted() in
      let* r = setUp r in
      let* r = main r in
      let* r = tearDown r in
      finalValueGetter r
    end in let (state, r) = packagedProgram init in r
end


module TestCaseTest = struct
  let setUp () = fun (_) -> ((), ())
  let tearDown () = fun (_) -> ((), ())
  let init = ()

  let testTemplateMethod () = fun (_) ->
    let result = WasRun.run WasRun.getLog WasRun.testMethod in
    let _ = asrt ("setUp testMethod tearDown " = result, "It should have run the correct methods. Methods called were: "^result) in
    ((), ())

  let testResult () = fun (_) ->
    let resultSummaryGetter () = begin
      let (let*) = WasRun.( >>= ) in
      let* result = WasRun.getResult () in
      WasRun.return @@ Result.getSummary result
    end in
    let result = WasRun.run resultSummaryGetter WasRun.testMethod in
    let _ = asrt ("1 run, 0 failed" = result, "It should have returned the correct result summary. Returned summary was: "^result) in
    ((), ())

  let ( >>= ) m f = fun s ->
      let (s', v) = m s in
      f v s'
  let run program =
    let go = begin
      let (let*) = ( >>= ) in
      let* r1 = setUp() in
      let* r2 = program r1 in
      tearDown r2
    end in
    go init
end

let _ = TestCaseTest.run TestCaseTest.testTemplateMethod
let _ = TestCaseTest.run TestCaseTest.testResult
