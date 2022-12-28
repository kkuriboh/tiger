open tiger.lexer
open tiger.excercises

// a := 5 + 3; b := (print (a, a - 1), 10 * a); print(b)
let prog =
    CompoundStm(
        AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
        CompoundStm(
            AssignStm(
                "b",
                EseqExp(
                    PrintStm[IdExp "a"
                             OpExp(IdExp "a", Minus, NumExp 1)],
                    OpExp(NumExp 10, Times, IdExp "a")
                )
            ),
            PrintStm([ IdExp "b" ])
        )
    )

printfn "%d" (maxargs prog)
interp prog

let test_tree =
    Tree.init "t"
    |> Tree.insert "s"
    |> Tree.insert "p"
    |> Tree.insert "i"
    |> Tree.insert "p"
    |> Tree.insert "f"
    |> Tree.insert "b"
    |> Tree.insert "s"
    |> Tree.insert "t"

printfn "%A" test_tree
printfn "contains 15: %b\ncontains 19: %b" (test_tree |> Tree.contains "p") (test_tree |> Tree.contains "Z")

let rnd = new System.Random()

let test_kvtree =
    KVTree.init "a" (rnd.Next(1, 1000))
    |> KVTree.insert "b" (rnd.Next(1, 1000))
    |> KVTree.insert "c" (rnd.Next(1, 1000))
    |> KVTree.insert "d" (rnd.Next(1, 1000))
    |> KVTree.insert "e" (rnd.Next(1, 1000))
    |> KVTree.insert "f" (rnd.Next(1, 1000))
    |> KVTree.insert "g" (rnd.Next(1, 1000))
    |> KVTree.insert "h" (rnd.Next(1, 1000))
    |> KVTree.insert "i" (rnd.Next(1, 1000))

printfn "%100A" test_kvtree
printfn "lookup for 'd': %A" (test_kvtree |> KVTree.lookup "d")
