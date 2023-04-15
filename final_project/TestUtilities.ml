module TestUtilities = struct
  let asrt = function
   | (true,_) -> ()
   | (false, str) -> failwith ("Assertion failure: "^str)
end
