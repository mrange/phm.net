open System
open System.Threading

let waitForDebugger () =
  printfn "Attach debugger and hit return..."
  Console.ReadLine () |> ignore
  printfn "Starting in 3 seconds..."
  Thread.Sleep 3000
  printfn "Go!!"


[<EntryPoint>]
let main argv =
  try
    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory
    // waitForDebugger ()
    CheckPerformance.run ()
    0
  with
  | e ->
    printfn "Caught: %s" e.Message
    999
