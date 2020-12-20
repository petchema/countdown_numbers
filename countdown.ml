(*
  with
  two "big numbers" among 25, 50, 75, 100
  and four "smaller numbers" among 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
  and the four standard operations (+, -, *, /)
  obtain some random target number between 100 and 999
  with no negative or fractional intermediate results
 *)

type operation =
  Value of int
| Add of (operation * operation)
| Substract of (operation * operation)
| Multiply of (operation * operation)
| Divide of (operation * operation)

type result = {
    value: int;
    computation: operation
  }

let card_of_int x = { value = x; computation = Value x }

let addition first_operand second_operand =
  { value = first_operand.value + second_operand.value;
    computation = Add (first_operand.computation, second_operand.computation) }

let substraction first_operand second_operand =
  { value = first_operand.value - second_operand.value;
    computation = Substract (first_operand.computation, second_operand.computation) }

let multiplication first_operand second_operand =
  { value = first_operand.value * second_operand.value;
    computation = Multiply (first_operand.computation, second_operand.computation) }

let division first_operand second_operand =
  { value = first_operand.value / second_operand.value;
    computation = Divide (first_operand.computation, second_operand.computation) }

let rec string_of_operation op =
  match op with
  | Value x -> Printf.sprintf "%d" x
  | Add (a, b) -> Printf.sprintf "(%s + %s)" (string_of_operation a) (string_of_operation b)
  | Substract (a, b) -> Printf.sprintf "(%s - %s)" (string_of_operation a) (string_of_operation b)
  | Multiply (a, b) -> Printf.sprintf "(%s * %s)" (string_of_operation a) (string_of_operation b)
  | Divide (a, b) -> Printf.sprintf "(%s / %s)" (string_of_operation a) (string_of_operation b)

let natural_string_of_operation op = (* remove unnecessary ()s *)
  let rec aux op priority =
    match op with
    | Value x -> Printf.sprintf "%d" x
    | Add (a, b) -> (if priority > 0 then Printf.sprintf "(%s + %s)" else Printf.sprintf "%s + %s") (aux a 0) (aux b 0)
    | Substract (a, b) -> (if priority > 1 then Printf.sprintf "(%s - %s)" else Printf.sprintf "%s - %s") (aux a 0) (aux b 1)
    | Multiply (a, b) -> (if priority > 2 then Printf.sprintf "(%s * %s)" else Printf.sprintf "%s * %s") (aux a 2) (aux b 2)
    | Divide (a, b) -> (if priority > 3 then Printf.sprintf "(%s / %s)" else Printf.sprintf "%s / %s") (aux a 2) (aux b 3) in
  aux op 0

(* picking each element of a list in turn, call a function with that element, the elements already processed, the elements yet to process,
   and an accumulator, folding the result as you go *)
let pick_each list acc f =
  let rec aux remaining elt_done acc =
  match remaining with
  | [] -> acc
  | h :: q ->
     aux q (h :: elt_done) (f h q elt_done acc) in
  aux list [] acc

let all_results cards =
  let rec aux remaining_cards results_list =
    pick_each remaining_cards results_list (fun first_operand remaining_cards done_cards1 results_list ->
        pick_each remaining_cards results_list (fun second_operand remaining_cards done_cards2 results_list ->
            let other_cards = List.rev_append done_cards1 (List.rev_append done_cards2 remaining_cards) in
            
            let add_additions results_list =
              if first_operand.value <> 0 && second_operand.value <> 0 then (* remove trivial operations *)
                add_result (addition first_operand second_operand) other_cards results_list else results_list in
            
            let add_substractions results_list =
              if first_operand.value >= second_operand.value then (* positive intermediate result only *)
                if second_operand.value <> 0 then (* remove trivial operations *)
                  add_result (substraction first_operand second_operand) other_cards results_list else results_list
              else if first_operand.value <> 0 then (* remove trivial operations *)
                add_result (substraction second_operand first_operand) other_cards results_list else results_list in

            let add_multiplications results_list =
              if first_operand.value <> 1 && second_operand.value <> 1 then (* remove trivial operations *)
                add_result (multiplication first_operand second_operand) other_cards results_list else results_list in

            let add_divisions results_list =
              results_list
              |> (fun results_list ->
                if second_operand.value > 1 &&  (* remove trivial operations *)
                     first_operand.value mod second_operand.value = 0 then (* no fractional intermediate result *)
                  add_result (division first_operand second_operand) other_cards results_list else results_list)
              |> (fun results_list ->
                if first_operand.value > 1 &&  (* remove trivial operations *)
                     second_operand.value mod first_operand.value = 0 then (* no fractional intermediate result *)
                  add_result (division second_operand first_operand) other_cards results_list else results_list) in
            
            results_list
            |> add_additions
            |> add_substractions
            |> add_multiplications
            |> add_divisions
      ))
  and add_result new_result remaining_cards results_list = (* add operation result to the list of results and continue *)
    aux (new_result :: remaining_cards) (new_result :: results_list) in
  let cards = List.map card_of_int cards in
  aux cards cards (* add each card to the list of results *)

  

module Result = struct
  type t = result
  let compare a b =
    let c = compare a b in
    if c <> 0 then c
    else
      compare a.computation b.computation
end

module UniqueResults = Set.Make(Result)

let results = all_results [100;1;2;5;4;7]

let () =
  Printf.printf "Computations: %d\n" (List.length results)

let () =
  results
  |> List.rev_map (fun result -> Printf.sprintf "%s = %d" (string_of_operation result.computation) result.value)
  |> List.iter print_endline

let unique_results = List.fold_left (fun acc result -> UniqueResults.add result acc) UniqueResults.empty results

let () =
  print_newline ();
  Printf.printf "Unique computations: %d\n" (UniqueResults.cardinal unique_results)

let () =
  UniqueResults.elements unique_results
  |> List.rev_map (fun result -> Printf.sprintf "%s = %d" (string_of_operation result.computation) result.value)
  |> List.iter print_endline
                                                                                                   

let results_226 = UniqueResults.elements unique_results |> List.filter (fun result -> result.value = 226)

let () =
  print_newline ();
  Printf.printf "Result for 226: %d\n" (List.length results_226)

let () =
  List.rev_map (fun x -> x.computation) results_226
  |> List.rev_map (fun op -> Printf.sprintf "%s\t[%s]" (natural_string_of_operation op) (string_of_operation op))
  |> List.iter print_endline
