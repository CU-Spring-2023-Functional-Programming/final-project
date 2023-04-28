module FailedSetup = struct
  let getLog = fun (state, testResult) -> state

  let testMethod = fun (state, testResult) -> (state^"testMethod ", testResult)

  include TestCase(struct
    type stateType = string

    let initState = ""
    let setUp = fun (state, testResult) -> failwith "Broken setup"
    let tearDown = fun (state, testResult) -> (state^"tearDown ", testResult)
  end)

  let tests = [
    run "FailedSetup.testMethod" testMethod;
  ]
end
