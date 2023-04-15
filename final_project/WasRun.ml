module WasRun = struct
  let stateKeyLog = "log"
  let getLog = fun (state, testResult) -> (state, testResult, state)

  let testMethod = fun (state, testResult) -> (state^"testMethod ", testResult, ())

  let testBrokenMethod = fun _ -> failwith "Broken method"

  include TestCase(struct
    type stateType = string
    type testResultType = int * int

    let initState = ""
    let setUp = fun (state, testResult) -> (state^"setUp ", testResult, ())
    let tearDown = fun (state, testResult) -> (state^"tearDown ", testResult, ())
  end)
end
