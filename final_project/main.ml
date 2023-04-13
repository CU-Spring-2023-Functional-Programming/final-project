let asrt = function
 | (true,_) -> ()
 | (false, str) -> failwith ("Assertion failure: "^str)


module WasRun = struct
  let setUp = fun (wasRun, _) -> ((wasRun, true), ())

  let wasRun () = fun (wasRun, wasSetUp) -> ((wasRun, wasSetUp), wasRun)
  let wasSetUp () = fun (wasRun, wasSetUp) -> ((wasRun, wasSetUp), wasSetUp)
  let testMethod = fun (_, wasSetUp) -> ((true, wasSetUp), ())

  let init = (false, false)
  let run program =
    let (state1, _) = setUp init in
    let (state2, result) = program state1 in result

  let ( >>= ) m f = fun s ->
      let (s', v) = m s in
      f v s'
end


module TestCaseTest = struct
    let setUp = fun (_) ->
        let (let*) = WasRun.( >>= ) in
        let program = begin
            let* testMethod = WasRun.testMethod in
            WasRun.wasRun testMethod
        end in (program, ())

    let testRunning = fun program ->
        let _ = asrt (WasRun.run program, "It should have run") in
        (program, ())

    let testSetUp = fun program ->
        let _ = asrt (WasRun.run program, "It should have been set up") in
        (program, ())

  let init = ()
  let run program =
    let (state1, _) = setUp init in
    let (state2, result) = program state1 in result

  let ( >>= ) m f = fun s ->
      let (s', v) = m s in
      f v s'
end

let _ = TestCaseTest.run TestCaseTest.testRunning
let _ = TestCaseTest.run TestCaseTest.testSetUp
