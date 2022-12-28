module tiger.excercises

open tiger.lexer

let maxargs stmt =
    let rec match_stmt stmt lens =
        match stmt with
        | CompoundStm(first, second) -> match_stmt first lens @ match_stmt second lens
        | AssignStm(_, expr) -> match_exp expr lens
        | PrintStm args -> args.Length :: (args |> List.collect (fun x -> match_exp x lens))

    and match_exp expr lens =
        match expr with
        | OpExp(left, _, right) -> match_exp left lens @ match_exp right lens
        | EseqExp(first, second) -> match_stmt first lens @ match_exp second lens
        | _ -> lens

    (match_stmt stmt []) |> List.max

let interp stmt =
    let update value list = value :: list

    let lookup id (table: List<id * int>) =
        let (_, res) = table |> List.find (fun (val_id, _) -> val_id = id)
        res

    let get_operator binop =
        match binop with
        | Plus -> (+)
        | Minus -> (-)
        | Times -> (*)
        | Div -> (/)

    let rec interp_stm stmt state =
        match stmt with
        | CompoundStm(first, second) -> interp_stm first state |> interp_stm second
        | AssignStm(id, exp) ->
            let res, new_state = interp_exp exp state
            update (id, res) new_state
        | PrintStm args ->
            let rec reduce acc (index: int) =
                if index < args.Length then
                    let (res, cur_state) = interp_exp args[index] state
                    printfn "%d" res
                    reduce cur_state (index + 1)
                else
                    acc

            reduce state 0

    and interp_exp expr state =
        match expr with
        | OpExp(left, operator, right) ->
            let (first_res, first_state) = interp_exp left state
            let (second_res, second_state) = interp_exp right first_state
            (second_res |> get_operator operator first_res, second_state)
        | EseqExp(first, second) -> interp_stm first state |> interp_exp second
        | NumExp num -> (num, state)
        | IdExp id -> (state |> lookup id, state)

    let _ = interp_stm stmt []
    ()

type tree<'key> =
    | Leaf
    | Tree of tree<'key> * 'key * tree<'key>

module Tree =
    let init key = Tree(Leaf, key, Leaf)

    let rec insert key tree =
        match tree with
        | Leaf -> Tree(Leaf, key, Leaf)
        | Tree(left, _key, right) ->
            if _key < key then Tree(insert key left, _key, right)
            elif _key > key then Tree(left, _key, insert key right)
            else Tree(left, _key, right)

    // member is a keyword
    let rec contains key tree =
        match tree with
        | Leaf -> false
        | Tree(left, _key, right) ->
            if key = _key then
                true
            else
                contains key left || contains key right

type kvtree<'k, 'v> =
    | KVLeaf
    | KVTree of KVTree<'k, 'v>

and KVTree<'k, 'v> =
    { left: kvtree<'k, 'v>
      key: 'k
      value: 'v
      right: kvtree<'k, 'v> }

module KVTree =
    let init key value =
        KVTree
            { left = KVLeaf
              key = key
              value = value
              right = KVLeaf }

    let rec insert key value kvtree =
        match kvtree with
        | KVLeaf ->
            KVTree
                { left = KVLeaf
                  key = key
                  value = value
                  right = KVLeaf }
        | KVTree tree ->
            if tree.key < key then
                KVTree
                    { left = tree.left
                      key = tree.key
                      value = tree.value
                      right = tree.right |> insert key value }
            elif tree.key > key then
                KVTree
                    { left = tree.left |> insert key value
                      key = tree.key
                      value = tree.value
                      right = tree.right }
            else
                KVTree
                    { left = tree.left
                      key = tree.key
                      value = tree.value
                      right = tree.right }

    let rec lookup key kvtree =
        match kvtree with
        | KVLeaf -> None
        | KVTree tree ->
            if tree.key < key then tree.right |> lookup key
            elif tree.key > key then tree.left |> lookup key
            else Some tree.value
