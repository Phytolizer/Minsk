open System
open Minsk

let rec repl () =
    Console.Write "> "
    let line = Console.ReadLine () in
        match line with
        | null -> ()
        | line ->
            Console.WriteLine line
            repl ()

in repl ()
