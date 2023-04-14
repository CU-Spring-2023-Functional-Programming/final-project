let asrt = function
 | (true,_) -> ()
 | (false, str) -> failwith ("Assertion failure: "^str)


module Result = struct
  let init = ((), ())

  let getSummary () = fun (_, _) -> ((), "1 run, 0 failed")

  let ( >>= ) m f = fun s ->
      let (s', v) = m s in
      f v s'
  let run main =
    let (state, r) = main init in
    r
end


module WasRun = struct
  let setUp () = fun (wasRun, log) -> ((wasRun, log^"setUp "), ())
  let tearDown () = fun (wasRun, log) -> ((wasRun, log^"tearDown "), ())
  let init = (false, "")

  let getWasRun () = fun (wasRun, log) -> ((wasRun, log), wasRun)
  let getLog () = fun (wasRun, log) -> ((wasRun, log), log)
  let getResult () = fun state -> (state, Result.run (Result.getSummary()))

  let testMethod () = fun (_, log) -> ((true, log^"testMethod "), ())

  let ( >>= ) m f = fun s ->
      let (s', v) = m s in
      f v s'
  let run finalValueGetter main =
    let packagedProgram = begin
      let (let*) = ( >>= ) in
      let* r = setUp() in
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
    let result = WasRun.run WasRun.getResult WasRun.testMethod in
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
