module TestSuite = struct
  let initState = ([])

  let add newTest = fun (tests, testResult) -> (newTest :: tests, testResult, ())

  let getResult = fun (state, testResult) -> (state, testResult, testResult)
  let ( >>= ) m f = fun s ->
    let (s', r, v) = m s in
    f v (s', r)
  let run finalValueGetter main testResult =
    let (tests, testResult, ()) = main (initState, testResult) in
    let testResult = List.fold_left (fun currentResults test -> test currentResults) testResult tests in
    let (tests, testResult, v) = finalValueGetter (tests, testResult) in
    v

  let rec fromTests tests = match tests with
    | lastTest::[] -> add lastTest
    | _ ->
      let (let*) = ( >>= ) in
      let* _ = add (List.hd tests) in
      fromTests (List.tl tests);;
end
