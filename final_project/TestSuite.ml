module TestSuite = struct
  let initState = ([])

  let add newTest = fun (tests, testResult) -> (newTest :: tests, testResult)

  let getResult = fun (state, testResult) -> testResult
  let ( >>= ) m f = fun s ->
    let s' = m s in
    f () s'
  let run main testResult =
    let (tests, testResult) = main (initState, testResult) in
    let testResult = List.fold_left (fun currentResults test -> let (state, res) = test currentResults in res) testResult tests in
    (tests, testResult)

  let rec fromTests tests = match tests with
    | lastTest::[] -> add lastTest
    | _ ->
      let ( let* ) = ( >>= ) in
      let* _ = add (List.hd tests) in
      fromTests (List.tl tests)
end
