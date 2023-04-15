#use "TestResult.ml";;

module WasRun = struct
  let setUp () = fun ((wasRun, log), testResult) -> ((wasRun, log^"setUp "), testResult, ())
  let tearDown () = fun ((wasRun, log), testResult) -> ((wasRun, log^"tearDown "), testResult, ())
  let initState = (false, "")

  let getWasRun () = fun ((wasRun, log), testResult) -> ((wasRun, log), testResult, wasRun)
  let getLog () = fun ((wasRun, log), testResult) -> ((wasRun, log), testResult, log)
  let getResult () = fun ((wasRun, log), testResult) -> ((wasRun, log), testResult, testResult)

  let testMethod () = fun ((_, log), testResult) -> ((true, log^"testMethod "), testResult, ())

  let testBrokenMethod () = fun _ -> failwith "Broken method"

  let init = (initState, TestResult.init)
  let recordStarted () = fun (state, testResult) -> (state, TestResult.testStarted(testResult), ())
  let recordFailed () = fun (state, testResult) -> (state, TestResult.testFailed(testResult), ())
  let return v = fun (state, testResult) -> (state, testResult, v)
  let ( >>= ) m f = fun s ->
    let (s', r, v) = m s in
    f v (s', r)
  let run finalValueGetter main =
    let packagedProgram = begin
      let (let*) = ( >>= ) in
      let* v = recordStarted() in
      let* v = setUp v in
      let* v = fun s ->
        try
        	main v s
        with
        	| _ -> recordFailed() s
      in
      let* v = tearDown v in
      finalValueGetter v
    end in let (state, testResult, v) = packagedProgram init in v
end
