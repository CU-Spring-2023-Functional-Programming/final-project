#use "Result.ml";;

module WasRun = struct
  let setUp () = fun (wasRun, log, result) -> ((wasRun, log^"setUp ", result), ())
  let tearDown () = fun (wasRun, log, result) -> ((wasRun, log^"tearDown ", result), ())
  let init = (false, "", Result.init)

  let getWasRun () = fun (wasRun, log, result) -> ((wasRun, log, result), wasRun)
  let getLog () = fun (wasRun, log, result) -> ((wasRun, log, result), log)
  let getResult () = fun (wasRun, log, result) -> ((wasRun, log, result), result)

  let recordStarted () = fun (wasRun, log, result) -> ((wasRun, log, Result.incrementRunCount(result)), ())

  let testMethod () = fun (_, log, result) -> ((true, log^"testMethod ", result), ())

  let return r = fun state -> (state, r)
  let ( >>= ) m f = fun s ->
      let (s', v) = m s in
      f v s'
  let run finalValueGetter main =
    let packagedProgram = begin
      let (let*) = ( >>= ) in
      let* r = recordStarted() in
      let* r = setUp r in
      let* r = main r in
      let* r = tearDown r in
      finalValueGetter r
    end in let (state, r) = packagedProgram init in r
end
