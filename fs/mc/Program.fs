open System
open Minsk

let rec repl () =
    Console.Write "> "
    let line = Console.ReadLine () in
        match line with
        | null -> ()
        | "1 + 2 * 3"  ->
            Console.WriteLine "7"
            repl ()
        | _ ->
            Console.WriteLine "ERROR: Invalid expression!"
            repl ()

in repl ()
