open System

[<EntryPoint>]
let main argv =
  try
    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

    // CheckProperties.Properties.``PHM to* must contain all added values`` [||] |> printfn "%A"
    CheckProperties.run ()
  #if !DEBUG
//    PerformanceTests.run ()
  #endif
    0
  with
  | e ->
    printfn "Caught: %s" e.Message
    999
