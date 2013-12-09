(* Derek Redfern
 * dredfern.olin@gmail.com
 * FOCS Homework 6
 * 10.25.13
 *
 * Collaboration: [none]
 *
 *)


(* CODE FOR HOMEWORK 6
 *
 * Deterministic Turing machines
 *
 *)


let fail s = raise (Failure s)

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
    else loop (index-1) ((String.get str index)::result)  in
  loop (String.length str - 1) []

let implode (cs) = 
  let str = String.create(List.length(cs))  in
  (List.iteri (String.set str) cs; str)



(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *)

type direction = Left | Right

type 'a turing = { states : 'a list;
           input_alph : char list;
           tape_alph : char list;
           leftmost : char;
           blank : char;
           delta : ('a * char) -> ('a * char * direction);
           start : 'a;
           accept : 'a;
           reject : 'a }


(*
 * Print a configuration (including newline) to standard output
 * 
 *)

let print_config m (u,q,v) = 
  let string cs =
    let str = String.make (List.length(cs) * 2) ' '  in
    let put i = String.set str (i*2)  in
    (List.iteri put cs; str)  in
  Printf.printf "  %s(%s) %s\n" (string u) q (string v)



(*
 * IMPLEMENT THE FOLLOWING FUNCTIONS FOR QUESTION 2
 *
 *)

let starting_config tm input = ([],tm.start,tm.leftmost::(explode input));;

let accepting_config tm (_,state,_) = tm.accept = state;;

let rejecting_config tm (_,state,_) = tm.reject = state;;

let halting_config tm config = ((rejecting_config tm config) || (accepting_config tm config));;

let last lst = (List.nth lst ((List.length lst)-1));;

let allButLast lst = 
    (List.flatten 
        (List.mapi 
            (fun i x -> if (i=((List.length lst)-1)) then [] else [x]) 
            lst
        )
    )
;;

(* NOTE: if right is empty, then we must insert a blank *)
let step_config tm (left,state,right) = 
    let right' = (if (List.length right) = 0 then [tm.blank] else right) in
        let (newstate,newchar,dir) = (tm.delta (state,(List.hd right'))) in
            if (dir=Left) then
                ((allButLast left),newstate,(last left)::(newchar::(List.tl right')))
            else
                (left@[newchar],newstate,(List.tl right'))
;;
    
let rec cycle tm config = begin
    (print_config tm config);
    if (accepting_config tm config) then true
    else if (rejecting_config tm config) then false
    else 
        (cycle tm (step_config tm config))
end ;;

let run tm input = (cycle tm (starting_config tm input));;





(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the context-free language {a^n b^n | n >= 0}
 * anbncn is the non-context-free language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { states = ["start"; "q1"; "acc"; "rej"];
         input_alph = ['a';'b'];
         tape_alph = ['a';'b';'_';'>'];
         blank = '_';
         leftmost = '>';
         start = "start";
         accept = "acc";
         reject = "rej";
         delta = fun (q,a) -> 
                       match (q,a) with
                       | ("start", 'a') -> ("start", 'a', Right)
                       | ("start", 'b') -> ("q1", 'b', Right)
               | ("start", '>') -> ("start", '>', Right)
               | ("start", '_') -> ("acc", '_', Right)
               | ("q1", 'a') -> ("rej", 'a', Right)
               | ("q1", 'b') -> ("q1", 'b', Right)
               | ("q1", '>') -> ("rej", '>', Right)
               | ("q1", '_') -> ("acc", '_', Right)
               | ("acc", sym) -> ("acc", sym, Right)
               | ("rej", sym) -> ("rej", sym, Right)
               | _ -> fail "Undefined transition" }

let anbn = { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
         input_alph = ['a';'b'];
         tape_alph = ['a';'b';'X';'_';'>'];
         blank = '_';
         leftmost = '>';
         start = "start";
         accept = "acc";
         reject = "rej";
         delta = fun (q,a) -> 
                   match (q,a) with
               | ("start", 'a') -> ("start", 'a', Right)
                       | ("start", 'b') -> ("q1", 'b', Right)
               | ("start", '>') -> ("start", '>', Right)
               | ("start", '_') -> ("q2", '_', Right)
               | ("start", 'X') -> ("rej", 'X', Right)
               | ("q1", 'b') -> ("q1", 'b', Right)
               | ("q1", '_') -> ("q2", '_', Right)
               | ("q1", sym) -> ("rej", sym, Right)
               | ("q2", '>') -> ("q3", '>', Right)
               | ("q2", sym) -> ("q2", sym, Left)
               | ("q3", 'X') -> ("q3", 'X', Right)
               | ("q3", '_') -> ("acc", '_', Right)
               | ("q3", 'a') -> ("q4", 'X', Right)
               | ("q3", sym) -> ("rej", sym, Right)
               | ("q4", 'a') -> ("q4", 'a', Right)
               | ("q4", 'X') -> ("q4", 'X', Right)
               | ("q4", 'b') -> ("q2", 'X', Right)
               | ("q4", sym) -> ("rej", sym, Right)
               | ("acc", sym) -> ("acc", sym, Right)
               | ("rej", sym) -> ("rej", sym, Right)
               | _ -> fail "Undefined transition" }

let anbncn = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
           input_alph = ['a';'b';'c'];
           tape_alph = ['a';'b';'c';'X';'_';'>'];
           blank = '_';
           leftmost = '>';
           start = "start";
           accept = "acc";
           reject = "rej";
           delta = fun (q,a) -> 
             match (q,a) with
         | ("start", 'a') -> ("start", 'a', Right)
                 | ("start", 'b') -> ("q1", 'b', Right)
         | ("start", 'c') -> ("q6", 'c', Right)
         | ("start", '>') -> ("start", '>', Right)
         | ("start", '_') -> ("q2", '_', Right)
         | ("start", 'X') -> ("rej", 'X', Right)
         | ("q1", 'b') -> ("q1", 'b', Right)
         | ("q1", 'c') -> ("q6", 'c', Right)
         | ("q1", '_') -> ("q2", '_', Right)
         | ("q1", sym) -> ("rej", sym, Right)
         | ("q2", '>') -> ("q3", '>', Right)
         | ("q2", sym) -> ("q2", sym, Left)
         | ("q3", 'X') -> ("q3", 'X', Right)
         | ("q3", '_') -> ("acc", '_', Right)
         | ("q3", 'a') -> ("q4", 'X', Right)
         | ("q3", sym) -> ("rej", sym, Right)
         | ("q4", 'a') -> ("q4", 'a', Right)
         | ("q4", 'X') -> ("q4", 'X', Right)
         | ("q4", 'b') -> ("q5", 'X', Right)
         | ("q4", sym) -> ("rej", sym, Right)
         | ("q5", 'b') -> ("q5", 'b', Right)
         | ("q5", 'X') -> ("q5", 'X', Right)
         | ("q5", 'c') -> ("q2", 'X', Right)
         | ("q5", sym) -> ("rej", sym, Right)
         | ("q6", 'c') -> ("q6", 'c', Right)
         | ("q6", '_') -> ("q2", '_', Right)
         | ("q6", sym) -> ("rej", sym, Right)
         | ("acc", sym) -> ("acc", sym, Right)
         | ("rej", sym) -> ("rej", sym, Right)
         | _ -> fail "Undefined transition" }



(* 
 * The type for a DFA, parameterized by the type for the states 
 *
 *)

type 'a dfa = {states_d :   'a list;
               alphabet_d : char list;
           start_d :    'a;
           delta_d :    ('a * char * 'a) list;
           final_d :    'a list}



(*
 * Some sample DFAs
 *
 * isolatedBs: all strings where every b is bracketed by a's
 * asThenBs: strings of a's followed by b's
 *
 *)

let isolatedBs =    
  {alphabet_d = ['a'; 'b'];   
   states_d = ["start"; "readb"; "sink"];
   start_d = "start";
   delta_d = [("start", 'a', "start");
            ("start", 'b', "readb");
            ("readb", 'a', "start");
            ("readb", 'b', "sink"); 
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final_d = ["start";"readb"]}

let asThenBs =              
    {states_d = ["start"; "matchb"; "sink"];
     alphabet_d = ['a'; 'b'];
     start_d = "start";
     delta_d = [("start", 'a', "start");
              ("start", 'b', "matchb");
              ("matchb", 'a', "sink");
              ("matchb", 'b', "matchb");
              ("sink", 'a', "sink");
              ("sink", 'b', "sink")];
     final_d = ["start"; "matchb"]}


(* 
 * IMPLEMENT THE FOLLOWING FUNCTIONS FOR QUESTION 3
 *
 *)

let contains lst item = List.fold_right
  (fun x r -> if x=item then true else r)
  lst false;;

let deltaGen dfa (q,a) =
    let isFinal = (contains dfa.final_d q) in
        let isBlank = (a='_') in
            if(isFinal && isBlank) then
                ("acc",a,Right)
            else if (isBlank) then
                ("rej",a,Right)
            else if (a='>') then
                (q,a,Right)
            else ((List.fold_right 
                    (fun (s1,c,s2) r -> if (s1=q && a=c) then s2 else r) 
                    dfa.delta_d 
                    "rej"),
                 a,Right)
;;

let turing_DFA dfa = { 
    states = dfa.states_d;
    input_alph = dfa.alphabet_d;
    tape_alph = dfa.alphabet_d@['_';'>'];
    blank = '_';
    leftmost = '>';
    start = "start";
    accept = "acc";
    reject = "rej";
    delta = (deltaGen dfa)
};;

(* Photos of this Turing machine drawn out:
Check string form - https://www.dropbox.com/s/ufgxkhfjbpbssoo/2013-11-06%2015.27.30.jpg
Everything else - https://www.dropbox.com/s/3l3li0azyx8gvo1/2013-11-06%2015.26.35.jpg
Everything else (first half zoomed) - https://www.dropbox.com/s/cljy27jwmaoyyrf/2013-11-06%2015.26.46.jpg
Everything else (second half zoomed) - https://www.dropbox.com/s/p5vr5cnq77htyq0/2013-11-06%2015.26.53.jpg

My method for constructing this Turing machine:
    1. Check that the string is of the correct form
    2. For every symbol in w2, do the following repeatedly until all are marked as X's:
    2a. If 0, then replace with X and mark the symbol in the corresponding location in
        w3 (by replacing 0 with % and 1 with 1)
    2b. If 1, then replace with X. If the symbol in w3 is a 1, replace with %; otherwise,
        replace with ! and perform a ripple-borrow to the left
    3. Convert all the %s back to 0s and the !s back to 1s and repeat step 2 for w1
    4. Check that w1 is now equal to 000...

Note that essentially, I am subtracting both w1 and w2 from w3 and making sure w3=0 afterwards.

**** Note: my machine does work for strings where |w1|=/=|w2|=/=|w3|. :D ****

 *)
let binary_sum () = { 
    states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
    input_alph = ['1';'0';'#'];
    tape_alph = ['1';'0';'!';'%';'#';'X';'_';'>']; (* % stands for 0dot and ! stands for 1dot *)
    blank = '_';
    leftmost = '>';
    start = "start";
    accept = "acc";
    reject = "rej";
    delta = fun (q,a) -> 
             match (q,a) with

                (* start thru q5 check that the input is of the right form *)

                ("start", '>') -> ("start", '>', Right)
                | ("start", '0') -> ("q1", '0', Right)
                | ("start", '1') -> ("q1", '1', Right)
                | ("start", '#') -> ("rej", '#', Right)
                | ("start", '_') -> ("rej", '_', Right)
                | ("start", 'X') -> ("rej", 'X', Right)
                | ("start", '!') -> ("rej", '!', Right)
                | ("start", '%') -> ("rej", '%', Right)

                | ("q1", '>') -> ("rej", '>', Right)
                | ("q1", '0') -> ("q1", '0', Right)
                | ("q1", '1') -> ("q1", '1', Right)
                | ("q1", '#') -> ("q2", '#', Right)
                | ("q1", '_') -> ("rej", '_', Right)
                | ("q1", 'X') -> ("rej", 'X', Right)
                | ("q1", '!') -> ("rej", '!', Right)
                | ("q1", '%') -> ("rej", '%', Right)

                | ("q2", '>') -> ("rej", '>', Right)
                | ("q2", '0') -> ("q3", '0', Right)
                | ("q2", '1') -> ("q3", '1', Right)
                | ("q2", '#') -> ("rej", '#', Right)
                | ("q2", '_') -> ("rej", '_', Right)
                | ("q2", 'X') -> ("rej", 'X', Right)
                | ("q2", '!') -> ("rej", '!', Right)
                | ("q2", '%') -> ("rej", '%', Right)

                | ("q3", '>') -> ("rej", '>', Right)
                | ("q3", '0') -> ("q3", '0', Right)
                | ("q3", '1') -> ("q3", '1', Right)
                | ("q3", '#') -> ("q4", '#', Right)
                | ("q3", '_') -> ("rej", '_', Right)
                | ("q3", 'X') -> ("rej", 'X', Right)
                | ("q3", '!') -> ("rej", '!', Right)
                | ("q3", '%') -> ("rej", '%', Right)

                | ("q4", '>') -> ("rej", '>', Right)
                | ("q4", '0') -> ("q5", '0', Right)
                | ("q4", '1') -> ("q5", '1', Right)
                | ("q4", '#') -> ("rej", '#', Right)
                | ("q4", '_') -> ("rej", '_', Right)
                | ("q4", 'X') -> ("rej", 'X', Right)
                | ("q4", '!') -> ("rej", '!', Right)
                | ("q4", '%') -> ("rej", '%', Right)

                | ("q5", '>') -> ("rej", '>', Right)
                | ("q5", '0') -> ("q5", '0', Right)
                | ("q5", '1') -> ("q5", '1', Right)
                | ("q5", '#') -> ("rej", '#', Right)
                | ("q5", '_') -> ("q6", '_', Right)
                | ("q5", 'X') -> ("rej", 'X', Right)
                | ("q5", '!') -> ("rej", '!', Right)
                | ("q5", '%') -> ("rej", '%', Right)

                (* q6 finds the next character in w2 *)

                | ("q6", '>') -> ("rej", '>', Right)
                | ("q6", '0') -> ("q7", 'X', Left)
                | ("q6", '1') -> ("q11", 'X', Left)
                | ("q6", '#') -> ("q15", '#', Left)
                | ("q6", '_') -> ("q6", '_', Left)
                | ("q6", 'X') -> ("q6", 'X', Left)

                (* q7-q9 handle a 0 in w2 *)

                | ("q7", '>') -> ("rej", '>', Right)
                | ("q7", '0') -> ("q7", '0', Left)
                | ("q7", '1') -> ("q7", '1', Left)
                | ("q7", '#') -> ("q8", '#', Left)
                | ("q7", '_') -> ("rej", '_', Left)
                | ("q7", 'X') -> ("rej", 'X', Left)

                | ("q8", '>') -> ("rej", '>', Right)
                | ("q8", '0') -> ("q8", '0', Left)
                | ("q8", '1') -> ("q8", '1', Left)
                | ("q8", '#') -> ("q9", '#', Left)
                | ("q8", '_') -> ("rej", '_', Left)
                | ("q8", 'X') -> ("rej", 'X', Left)

                | ("q9", '>') -> ("q10", '>', Right)
                | ("q9", '0') -> ("q10", '%', Left)
                | ("q9", '1') -> ("q10", '!', Left)
                | ("q9", '%') -> ("q9", '%', Left)
                | ("q9", '!') -> ("q9", '!', Left)
                | ("q9", '#') -> ("rej", '#', Left)
                | ("q9", '_') -> ("rej", '_', Left)
                | ("q9", 'X') -> ("rej", 'X', Left)

                (* q11-q14 handle a 1 in w2 *)

                | ("q11", '>') -> ("rej", '>', Right)
                | ("q11", '0') -> ("q11", '0', Left)
                | ("q11", '1') -> ("q11", '1', Left)
                | ("q11", '#') -> ("q12", '#', Left)
                | ("q11", '_') -> ("rej", '_', Left)
                | ("q11", 'X') -> ("rej", 'X', Left)

                | ("q12", '>') -> ("rej", '>', Right)
                | ("q12", '0') -> ("q12", '0', Left)
                | ("q12", '1') -> ("q12", '1', Left)
                | ("q12", '#') -> ("q13", '#', Left)
                | ("q12", '_') -> ("rej", '_', Left)
                | ("q12", 'X') -> ("rej", 'X', Left)

                | ("q13", '>') -> ("rej", '>', Right)
                | ("q13", '0') -> ("q14", '!', Left)
                | ("q13", '1') -> ("q10", '%', Left)
                | ("q13", '%') -> ("q13", '%', Left)
                | ("q13", '!') -> ("q13", '!', Left)
                | ("q13", '#') -> ("rej", '#', Left)
                | ("q13", '_') -> ("rej", '_', Left)
                | ("q13", 'X') -> ("rej", 'X', Left)

                | ("q14", '>') -> ("rej", '>', Right)
                | ("q14", '0') -> ("q14", '1', Left)
                | ("q14", '1') -> ("q10", '0', Left)
                | ("q14", '%') -> ("rej", '%', Left)
                | ("q14", '!') -> ("rej", '!', Left)
                | ("q14", '#') -> ("rej", '#', Left)
                | ("q14", '_') -> ("rej", '_', Left)
                | ("q14", 'X') -> ("rej", 'X', Left)

                (* q10 resets the string to the end, then sends it to q6 *)

                | ("q10", '>') -> ("q10", '>', Right)
                | ("q10", '0') -> ("q10", '0', Right)
                | ("q10", '1') -> ("q10", '1', Right)
                | ("q10", '%') -> ("q10", '%', Right)
                | ("q10", '!') -> ("q10", '!', Right)
                | ("q10", '#') -> ("q10", '#', Right)
                | ("q10", '_') -> ("q6", '_', Right)
                | ("q10", 'X') -> ("q10", 'X', Right)

                (* q15 resets all %s to 0s and !s to 1s*)

                | ("q15", '>') -> ("q16", '>', Right)
                | ("q15", '0') -> ("q15", '0', Left)
                | ("q15", '1') -> ("q15", '1', Left)
                | ("q15", '%') -> ("q15", '0', Left)
                | ("q15", '!') -> ("q15", '1', Left)
                | ("q15", '#') -> ("q15", '#', Left)
                | ("q15", '_') -> ("rej", '_', Left)
                | ("q15", 'X') -> ("rej", 'X', Left)

                (* q16-q18 find the next character in w1 *)

                | ("q16", '>') -> ("q16", '>', Right)
                | ("q16", '0') -> ("q16", '0', Right)
                | ("q16", '1') -> ("q16", '1', Right)
                | ("q16", '%') -> ("q16", '%', Right)
                | ("q16", '!') -> ("q16", '!', Right)
                | ("q16", '#') -> ("q16", '#', Right)
                | ("q16", '_') -> ("q17", '_', Left)
                | ("q16", 'X') -> ("q16", 'X', Right)

                | ("q17", '>') -> ("rej", '>', Right)
                | ("q17", '0') -> ("rej", '0', Left)
                | ("q17", '1') -> ("rej", '1', Left)
                | ("q17", '%') -> ("rej", '0', Left)
                | ("q17", '!') -> ("rej", '1', Left)
                | ("q17", '#') -> ("q18", '#', Left)
                | ("q17", '_') -> ("rej", '_', Left)
                | ("q17", 'X') -> ("q17", 'X', Left)

                | ("q18", '>') -> ("rej", '>', Right)
                | ("q18", '0') -> ("q19", 'X', Left)
                | ("q18", '1') -> ("q21", 'X', Left)
                | ("q18", '%') -> ("rej", '%', Left)
                | ("q18", '!') -> ("rej", '!', Left)
                | ("q18", '#') -> ("q24", '#', Left)
                | ("q18", '_') -> ("rej", '_', Left)
                | ("q18", 'X') -> ("q18", 'X', Left)

                (* q19,q20 handle a 0 in w1 *)

                | ("q19", '>') -> ("rej", '>', Right)
                | ("q19", '0') -> ("q19", '0', Left)
                | ("q19", '1') -> ("q19", '1', Left)
                | ("q19", '#') -> ("q20", '#', Left)
                | ("q19", '_') -> ("rej", '_', Left)
                | ("q19", 'X') -> ("rej", 'X', Left)

                | ("q20", '>') -> ("q16", '>', Right)
                | ("q20", '0') -> ("q16", '%', Left)
                | ("q20", '1') -> ("q16", '!', Left)
                | ("q20", '%') -> ("q20", '%', Left)
                | ("q20", '!') -> ("q20", '!', Left)
                | ("q20", '#') -> ("rej", '#', Left)
                | ("q20", '_') -> ("rej", '_', Left)
                | ("q20", 'X') -> ("rej", 'X', Left)

                (* q21-q23 handle a 1 in w2 *)

                | ("q21", '>') -> ("rej", '>', Right)
                | ("q21", '0') -> ("q21", '0', Left)
                | ("q21", '1') -> ("q21", '1', Left)
                | ("q21", '#') -> ("q22", '#', Left)
                | ("q21", '_') -> ("rej", '_', Left)
                | ("q21", 'X') -> ("rej", 'X', Left)

                | ("q22", '>') -> ("rej", '>', Right)
                | ("q22", '0') -> ("q23", '!', Left)
                | ("q22", '1') -> ("q16", '%', Left)
                | ("q22", '%') -> ("q22", '%', Left)
                | ("q22", '!') -> ("q22", '!', Left)
                | ("q22", '#') -> ("rej", '#', Left)
                | ("q22", '_') -> ("rej", '_', Left)
                | ("q22", 'X') -> ("rej", 'X', Left)

                | ("q23", '>') -> ("rej", '>', Right)
                | ("q23", '0') -> ("q23", '1', Left)
                | ("q23", '1') -> ("q10", '0', Left)
                | ("q23", '%') -> ("rej", '%', Left)
                | ("q23", '!') -> ("rej", '!', Left)
                | ("q23", '#') -> ("rej", '#', Left)
                | ("q23", '_') -> ("rej", '_', Left)
                | ("q23", 'X') -> ("rej", 'X', Left)

                (* q24,q25 determine whether the addition was correct or not*)

                | ("q24", '>') -> ("q25", '>', Right)
                | ("q24", '0') -> ("q24", '0', Left)
                | ("q24", '1') -> ("q24", '1', Left)
                | ("q24", '%') -> ("q24", '0', Left)
                | ("q24", '!') -> ("q24", '1', Left)
                | ("q24", '#') -> ("rej", '#', Left)
                | ("q24", '_') -> ("rej", '_', Left)
                | ("q24", 'X') -> ("rej", 'X', Left)

                | ("q25", '>') -> ("rej", '>', Right)
                | ("q25", '0') -> ("q25", '0', Right)
                | ("q25", '1') -> ("rej", '1', Right)
                | ("q25", '%') -> ("rej", '0', Right)
                | ("q25", '!') -> ("rej", '1', Right)
                | ("q25", '#') -> ("acc", '#', Right)
                | ("q25", '_') -> ("rej", '_', Right)
                | ("q25", 'X') -> ("rej", 'X', Right)

                | _ -> fail "Undefined transition"
};; 

(* dredfern - binary adder TM - test suite

   All tests labeled testF should be rejected;
    all tests labeled testT should be accepted.

   Tests T9 and T10 will only succeed if your TM
    handles |w3|=/=|w2|=/=|w1|.

   Tests F12, T7, and T8 are huge and will obscure output from the
    other tests.
*)
let testBin () =
    let bin = (binary_sum ()) in
    let testF0 = (run bin "##") in
    let testF1 = (run bin "1#1#") in
    let testF2 = (run bin "#1#1") in
    let testF3 = (run bin "1##1") in
    let testF4 = (run bin "!#!#!") in
    let testF5 = (run bin "000") in
    let testF6 = (run bin "000#000") in
    let testF7 = (run bin "000##000") in
    let testF8 = (run bin "001#000#000") in
    let testF9 = (run bin "011#001#000") in
    let testF10 = (run bin "011#001#001") in
    let testF11 = (run bin "110#001#001") in
    let testF12 = (run bin "11110#00111#10101") in
    let testT0 = (run bin "000#000#000") in
    let testT1 = (run bin "001#000#001") in
    let testT2 = (run bin "001#001#000") in
    let testT3 = (run bin "001#000#001") in
    let testT4 = (run bin "010#001#001") in
    let testT5 = (run bin "110#101#001") in
    let testT6 = (run bin "111#101#010") in
    let testT7 = (run bin "11100#00111#10101") in
    let testT8 = (run bin "1001011111#010110111#110101000") in
    let testT9 = (run bin "111#101#10") in
    let testT10 = (run bin "101#00100#000001") in 

    begin 
        if (testF0 || testF1 || testF2 || testF3 || testF4 || testF5 || 
            testF6 || testF7 || testF8 || testF9 || testF10 || testF11 || testF12) then false
        else if ((not testT0) || (not testT1) || (not testT2) || (not testT3) || 
                    (not testT4) || (not testT5) || (not testT6) || (not testT7) || 
                    (not testT8) || (not testT9) || (not testT10) ) then false
        else true
    end
;;