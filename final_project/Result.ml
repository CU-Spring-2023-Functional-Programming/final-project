module Result = struct
  let init = (0, 0)

  let getSummary (runCount, failedCount) = (string_of_int runCount)^" run, 0 failed"

  let incrementRunCount (runCount, failedCount) = (runCount + 1, failedCount)
end
