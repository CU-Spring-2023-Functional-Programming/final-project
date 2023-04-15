module WasRun = struct
  let stateKeyLog = "log"
  let getLog = TestUtilities.getProperty stateKeyLog

  let testMethod () = fun (state, testResult) ->
    let (state, testResult, currentLog) = getLog () (state, testResult) in
    ([(stateKeyLog, currentLog^"testMethod ")], testResult, ())

  let testBrokenMethod () = fun _ -> failwith "Broken method"

  include TestCase(struct
    type stateType = (string * string) list
    type testResultType = int * int

    let initState = [(stateKeyLog, "")]
    let setUp () = fun (state, testResult) ->
      let (state, testResult, currentLog) = getLog () (state, testResult) in
      ([(stateKeyLog, currentLog^"setUp ")], testResult, ())
    let tearDown () = fun (state, testResult) ->
      let (state, testResult, currentLog) = getLog () (state, testResult) in
      ([(stateKeyLog, currentLog^"tearDown ")], testResult, ())
  end)
end
