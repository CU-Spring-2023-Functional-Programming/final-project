let asrt = function
 | (true,_) -> ()
 | (false, str) -> failwith ("Assertion failure: "^str)


module TestCase = struct
  let run run_state program = run_state program
end


module WasRun = struct
  let ( >>= ) m f = fun s ->
      let (s', v) = m s in
      f v s'

  let wasRun (numRunsCurrent) = fun numRuns -> (numRuns, numRunsCurrent > 0)
  let testMethod (_) = fun numRuns -> (numRuns + 1, numRuns + 1)

  let run_state i = let (s, v) = i (0) in v
end


module TestCaseTest = struct
    let (let*) = WasRun.( >>= )
    let testRunning () =
        let program = WasRun.wasRun 0 in
        let _ = asrt (not (TestCase.run WasRun.run_state program), "It should have begun with no runs") in
        let program =
            let* testMethod = WasRun.testMethod () in
            WasRun.wasRun testMethod in
        asrt (TestCase.run WasRun.run_state program, "It should have run at least once")

    let run_state i = i ()
end

let _ = TestCase.run TestCaseTest.run_state TestCaseTest.testRunning
