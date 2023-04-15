module TestUtilities = struct
  let asrt = function
   | (true,_) -> ()
   | (false, str) -> failwith ("Assertion failure: "^str)

  let getProperty name () = fun (state, testResult) -> (state, testResult, List.assoc name state)
end
