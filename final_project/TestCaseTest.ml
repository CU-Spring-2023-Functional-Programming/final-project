#use "WasRun.ml";;
#use "utilities.ml";;

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