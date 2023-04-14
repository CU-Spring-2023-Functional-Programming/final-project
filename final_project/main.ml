let asrt = function
 | (true,_) -> ()
 | (false, str) -> failwith ("Assertion failure: "^str)


module WasRun = struct
  let setUp () = fun (wasRun, log) -> ((wasRun, log^"setUp "), ())
  let tearDown () = fun (wasRun, log) -> ((wasRun, log^"tearDown "), ())
  let init = (false, "")

  let testMethod () = fun (_, log) -> ((true, log^"testMethod "), ())

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


module TestCaseTest = struct
    let setUp () = fun (_) -> ((), ())
    let tearDown () = fun (_) -> ((), ())
    let init = ()

    let getWasRun ((wasRun, log), ()) = wasRun
    let getLog ((wasRun, log), ()) = log
    let testTemplateMethod () = fun (_) ->
        let result = getLog (WasRun.run WasRun.testMethod) in
        let _ = asrt ("setUp testMethod tearDown " = result, "Should have run the correct methods. Methods called were: "^result) in
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
