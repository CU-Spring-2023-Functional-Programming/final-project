module WasRun = struct
  let getLog = fun (state, testResult) -> state

  let testMethod = fun (state, testResult) -> (state^"testMethod ", testResult)
  let testBrokenMethod = fun _ -> failwith "Broken method"

  include TestCase(struct
    type stateType = string

    let initState = ""
    let setUp = fun (state, testResult) -> (state^"setUp ", testResult)
    let tearDown = fun (state, testResult) -> (state^"tearDown ", testResult)
  end)

  let tests = [
    run "WasRun.testMethod" testMethod;
    run "WasRun.testBrokenMethod" testBrokenMethod;
  ]
end
