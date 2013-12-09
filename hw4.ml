(* Derek Redfern
 * dredfern.olin@gmail.com
 * FOCS Homework 4
 * 10.22.13
 *
 * Collaboration: [none]
 *
 *)


(* 
 * CODE FOR HOMEWORK 4
 *
 *)


(* 
 * String to Characters utility functions:
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode str = 
  let rec loop index result = 
    if (index<0) then result
    else loop (index-1) ((String.get str index)::result)
  in
    loop (String.length str - 1) []

let implode (cs) = 
  let str = String.create(List.length(cs))
  in (List.iteri (String.set str) cs; str)

let fail s = raise (Failure s)


(*
 * A function to perform depth first search in a tree
 * created from a transition relation.
 *
 * dfs trans init goal   returns true if there is a node N in the 
 *       tree created by the transition relation 'trans' starting
 *       at the initial node 'init' such that 'goal N' is true
 *
 * The maximum search depth is controlled by the integer in reference
 * cell 'dfs_threshold'. Update that value from the shell if you want
 * to search deeper in the tree. 
 * (You shouldn't need to unless you create your own rather involved 
 * examples.) 
 *
 *)

let dfs_threshold = ref 1000

let dfs trans init goal = 
  let rec loop seen stack = 
    match stack with
      [] -> false
    | (curr,d) :: stack' when d < !dfs_threshold 
                           && not (List.mem curr seen) -> 
           if (goal curr) then true
       else loop (curr::seen) ((List.map (fun x -> (x,d+1)) 
                                                      (trans curr))@stack')
    | (_,d) :: _ when d >= !dfs_threshold -> fail "DFS abort threshold reached"
    | _:: stack' -> loop seen stack'
  in loop [] [(init,0)]



(*
 * The type for PDAs
 * 
 *)

type 'a pda = {states : 'a list;
               input_alph : char list;
               delta : (('a * char option * char) * ('a * char list)) list;
               start : 'a;
               final : 'a list;
               stack_alph : char list;
               bottom : char}

(*
 * Create list of all strings of length <= n over a given alphabet
 *
 *)

let strings alphabet n = 
  let rec mapCons c = List.map (fun y -> c::y)  in
  let rec mapConsSet alphabet l = 
    List.fold_right (fun c -> List.append (mapCons c l)) alphabet []  in
  let rec strings' n =
    if (n<=0) then [[]]
    else [] :: mapConsSet alphabet (strings' (n-1))
  in List.map implode (strings' n)


(*
 * Some sample PDAs
 *
 *)


let anban = { states = ["q1"; "q2"; "q3"];
              input_alph = ['a';'b'];
              delta = [(("q1", Some 'a', '_'), ("q1", ['.'; '_']));
               (("q1", Some 'a', '.'), ("q1", ['.'; '.']));
               (("q1", Some 'b', '_'), ("q2", ['_']));
               (("q1", Some 'b', '.'), ("q2", ['.']));
               (("q2", Some 'a', '.'), ("q2", []));
               (("q2", None, '_'), ("q3", ['_']))];
          start = "q1";
          final = ["q3"];
          stack_alph = ['.'; '_'];
          bottom = '_' }

let anbn = { states = ["q1"; "q2"; "q3"];
              input_alph = ['a';'b'];
              delta = [(("q1", Some 'a', '_'), ("q1", ['.'; '_']));
               (("q1", Some 'a', '.'), ("q1", ['.'; '.']));
               (("q1", None, '_'), ("q2", ['_']));
               (("q1", None, '.'), ("q2", ['.']));
               (("q2", Some 'b', '.'), ("q2", []));
               (("q2", None, '_'), ("q3", ['_']))];
          start = "q1";
          final = ["q3"];
          stack_alph = ['.'; '_'];
          bottom = '_' }

let pal = { states = ["q1"; "q2"; "q3"];
            input_alph = ['a'; 'b'];
            delta = [(("q1", Some 'a', '_'), ("q1", ['a'; '_']));
             (("q1", Some 'a', 'a'), ("q1", ['a'; 'a']));
             (("q1", Some 'a', 'b'), ("q1", ['a'; 'b']));
             (("q1", Some 'b', '_'), ("q1", ['b'; '_']));
             (("q1", Some 'b', 'a'), ("q1", ['b'; 'a']));
             (("q1", Some 'b', 'b'), ("q1", ['b'; 'b']));
             (("q1", None, '_'), ("q2", ['_']));
             (("q1", None, 'a'), ("q2", ['a']));
             (("q1", None, 'b'), ("q2", ['b']));
             (("q2", Some 'a', 'a'), ("q2", []));
             (("q2", Some 'b', 'b'), ("q2", []));
             (("q2", None, '_'), ("q3", ['_']))];
            start = "q1";
            final = ["q3"];
            stack_alph = ['a'; 'b'; '_'];
            bottom = '_' }

let samenum = { states = ["q1"; "q2"];
        input_alph = ['a'; 'b'];
        delta = [(("q1", Some 'a', '_'), ("q1", ['a'; '_']));
             (("q1", Some 'a', 'a'), ("q1", ['a'; 'a']));
             (("q1", Some 'a', 'b'), ("q1", []));
             (("q1", Some 'b', '_'), ("q1", ['b'; '_']));
             (("q1", Some 'b', 'a'), ("q1", []));
             (("q1", Some 'b', 'b'), ("q1", ['b'; 'b']));
             (("q1", None, '_'), ("q2", ['_']))];
        start = "q1";
        final = ["q2"];
        stack_alph = ['a'; 'b'; '_'];
        bottom = '_' }

(* MY CODE STARTS HERE *)

let initial_config pda input = (pda.start,(explode input),[pda.bottom]);;

let contains lst item = List.fold_right
  (fun x r -> if x=item then true else r)
  lst false;;

let accepting_config pda (state,input,_) = ((contains pda.final state) && (input=[]));;

let filter_deltas (stateA,inputA,stackA) ((stateB,inputB,stackB),_) = 
    match inputA,inputB with
        Some optionA,Some optionB -> (stateA=stateB)&&(optionA=optionB)&&(stackA=stackB)
        | Some optionA, None -> (stateA=stateB)&&(stackA=stackB)
        | None, Some optionB -> false
        | None,None -> (stateA=stateB)&&(stackA=stackB)
;;

(* I am aware that this is not the most elegant method and that I should really do some checking
   before I blindly start popping things off of the input string and stack. However, it's 4am and
   I'm more interested in getting to bed... *)
let apply_deltas pda (inputPop,stackPop) ((_,trans,_),(statePush,stackPush)) = 
    match trans with
        None -> (statePush,inputPop,stackPush@(List.tl stackPop))
        | Some _ -> (statePush,(List.tl inputPop),stackPush@(List.tl stackPop))
;;

let step_config pda (state,input,stack) = 
    match input,stack with
        [],_ -> (List.map (apply_deltas pda (input,stack)) (List.filter (filter_deltas (state,None,(List.hd stack))) pda.delta))
        | _,_ -> (List.map (apply_deltas pda (input,stack)) (List.filter (filter_deltas (state,Some (List.hd input),(List.hd stack))) pda.delta));;

let accept pda input = (dfs (step_config pda) (pda.start,(explode input),[pda.bottom]) (accepting_config pda));;


(* 
 *  Compute the language of a PDA, restricted to inputs of length <= n
 *   language(dfa,n) returns a list of strings accepted by dfa
 *   printLanguage(dfa,n) prints the strings accepted by dfa
 *
 *)

let language pda n = 
  List.filter (accept pda) (strings pda.input_alph n)

let printLanguage pda n = 
  List.iter (fun s -> Printf.printf "  %s\n" (if (s="") then "<empty>" else s))
              (language pda n)