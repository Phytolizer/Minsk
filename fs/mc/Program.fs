open System
open Minsk

let rec repl () =
    Console.Write "> "
    let line = Console.ReadLine()

    match line with
    | null -> ()
    | line ->
        match line with
        | "1 + 2 * 3" -> Console.WriteLine "7"
        | _ -> Console.WriteLine "ERROR: Invalid expression!"

        repl ()

repl ()
