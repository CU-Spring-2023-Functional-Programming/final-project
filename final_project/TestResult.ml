module TestResult = struct
  let init = (0, 0)

  let getSummary (runCount, errorCount) = (string_of_int runCount)^" run, "^(string_of_int errorCount)^" failed"

  let testStarted (runCount, errorCount) = (runCount + 1, errorCount)
  let testFailed (runCount, errorCount) = (runCount, errorCount + 1)
end
