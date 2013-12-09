(* Derek Redfern
 * dredfern.olin@gmail.com
 * FOCS Homework 1
 * 9.22.13
 *
 * Collaboration: Brendan Ritter pointed me to this link for
 * more info on intersections:
 * http://www.cse.chalmers.se/~coquand/AUTOMATA/o2.pdf
 *)

open List;;

(* 
 * The type for a DFA, parameterized by the type for the states 
 *
 *)

type 'a dfa = {states :   'a list;
               alphabet : char list;
           start :    'a;
           delta :    ('a * char * 'a) list;
           final :    'a list}

(* 
 * String <-> characters utility functions:
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode (cs) = 
  let str = String.create(List.length(cs)) in
  let rec loop (cs,index) = 
    match cs with
      [] -> str
    | c::cs -> (String.set str index c; loop(cs,index+1))
  in
    loop(cs,0)

(*
 * Some error code
 * Call "transitionError" to report an error while looking for a 
 *   transition in the delta of a DFA
 *
 *)

exception DFAError of string

exception Unimplemented of string

let transitionError (input) = 
  raise (DFAError("Cannot transition on input "^(implode [input])))


(*
 * Some sample DFAs
 *
 *)


let isolatedBs =                (* language: all strings where every b *)
  {alphabet = ['a'; 'b'];       (* is bracketed by a's                 *)   
   states = ["start"; "readb"; "sink"];
   start = "start";
   delta = [("start", 'a', "start");
            ("start", 'b', "readb");
            ("readb", 'a', "start");
            ("readb", 'b', "sink"); 
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["start";"readb"]}


let asThenBs =                (* language: strings of a's followed by b's *)
    {states = ["eata"; "eatb"; "sink"];
     alphabet = ['a'; 'b'];
     start = "eata";
     delta = [("eata", 'a', "eata");
              ("eata", 'b', "eatb");
              ("eatb", 'a', "sink");
              ("eatb", 'b', "eatb");
              ("sink", 'a', "sink");
              ("sink", 'b', "sink")];
     final = ["eata"; "eatb"]}


let aStar =                    (* language: all strings of a's *)
  {alphabet= ['a'; 'b'];
   states= ["ok"; "sink"];
   start= "ok";
   delta = [("ok",   'a', "ok");
            ("ok",   'b', "sink");
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["ok"]}


let bPlus =                     (* language: all nonempty strings of b's *)
  {alphabet= ['a'; 'b'];
   states= ["start"; "ok"; "sink"];
   start= "start";
   delta = [("start", 'b', "ok");
            ("start", 'a', "sink");
            ("ok",    'b', "ok");
            ("ok",    'a', "sink");
            ("sink",  'b', "sink");
            ("sink",  'a', "sink")];
   final = ["ok"]}

let abaStar =              (* language: any number of ab's followed by a's *)
  {alphabet= ['a'; 'b'];
   states= ["astate"; "bstate"; "aonly"; "sink"];
   start= "astate";
   delta = [("astate", 'a', "bstate");
            ("astate", 'b', "sink");
            ("bstate", 'a', "aonly");
            ("bstate", 'b', "astate");
            ("aonly",  'a', "aonly");
            ("aonly",  'b', "sink");
            ("sink",   'a', "sink");
            ("sink",   'b', "sink")];
   final = ["astate"; "bstate"; "aonly"]}



(*
 * Create list of all strings of length <= n over a given alphabet
 *
 *)

let strings (alphabet, n) = 
  let rec mapCons (c, ls) = 
    match ls with
      [] -> []
    | l::ls' -> (c::l)::mapCons(c,ls')  in
  let rec mapConsSet (alphabet, l) = 
    match alphabet with
      [] -> []
    | c::cs -> mapCons(c,l) @ mapConsSet(cs,l)  in
  let rec mapImplode (css) = 
    match css with
      [] -> []
    | (cs::css) -> (implode cs)::mapImplode(css)  in
  let rec strings' (n) = 
    if (n<=0) then
      [[]]
    else let s = strings'(n-1) in
      [] :: mapConsSet(alphabet,s)
  in 
    mapImplode(strings'(n))

(*
 *  isFinal : 'a dfa * 'a -> bool
 *
 *    isFinal(dfa,q) should return true if and only if 'q' is a final state
 *    in the DFA 'dfa'
 *)

let rec contains (lst,item) =
    if ((List.length lst)=0) then
        false
    else if ((List.hd lst)=item) then
        true
    else
        contains((List.tl lst),item)
    ;;

let isFinal (dfa,state) =
    contains(dfa.final,state)
    ;;



(* 
 *  transition : 'a dfa * 'a * char -> 'a
 *
 *    transition(dfa,q,a) should return the state obtained by reading input
 *    symbol 'a' in state 'q' in the DFA 'dfa'
 *)

let rec findTransition (delta,state,input) =
    if ((List.length delta)=0) then
        transitionError(input)
    else begin
        let (deltaState,deltaInput,deltaNext) = (List.hd delta) in
            if(deltaState=state && deltaInput=input) then
                deltaNext
            else
                findTransition((List.tl delta),state,input)
    end
    ;;

let transition (dfa,state,input) =
    findTransition(dfa.delta,state,input);;



(*
 *  extendedTransition : 'a dfa * 'a * char list -> 'a
 *
 *    extendedTransition(dfa,q,cs) should return the state obtained by
 *    reading the list of input symbols in 'cs' from state 'q' in the DFA
 *    'dfa'
 *)

let rec extendedTransition(dfa,state,inputs) =
    if ((List.length inputs)=0) then
        state
    else if ((List.length inputs)=1) then
        transition(dfa,state,(List.hd inputs))
    else
        extendedTransition(dfa,transition(dfa,state,(List.hd inputs)),(List.tl inputs))
    ;;


(*
 *  accept : 'a dfa * string -> bool
 *
 *    accept(dfa,input) should return true if and only the input string
 *    'input' is accepted by the DFA 'dfa'
 *)

let accept (dfa,str) =
    (* print_string(str); *)
    isFinal(dfa,extendedTransition(dfa,dfa.start,(explode str)));;  

(* 
 *  Compute the language of a DFA, restricted to inputs of length <= n
 *   language(dfa,n) returns a list of strings accepted by dfa
 *   printLanguage(dfa,n) prints the strings accepted by dfa
 *
 *
 *)

let language (dfa, n) = 
  let candidates = strings(dfa.alphabet, n) in
  let rec tryAll (l) = 
    match l with
      [] -> []
    | s::ss -> if (accept(dfa,s)) then
                 s::(tryAll ss)
               else
                 tryAll ss
  in
    tryAll(candidates)


let printLanguage (dfa,n) = 
  let rec printList (l) = 
    match l with 
      [] -> ()
    | s::ss -> (print_string "   ";
                if (s="") then
                  print_string "<empty>"
                else
                  print_string s; 
                print_newline(); 
                printList ss)
  in
    printList(language(dfa,n))

(* Funnily enough, I already wrote this as a helper
 * function for part of 2. *)
let member (lst,item) =
    contains(lst,item);;

let rec difference (lst1,lst2) =
    if ((List.length lst1)=0) then
        []
    else begin
        if (contains(lst2, (List.hd lst1))) then
            difference((List.tl lst1),lst2)
        else
            (List.hd lst1)::difference((List.tl lst1),lst2)
    end
    ;;

let rec pair (item,lst) =
    if ((List.length lst)=0) then
        []
    else
        [(item,(List.hd lst))]@pair(item,(List.tl lst))

let rec cross (lst1,lst2) =
    if (((List.length lst1)=0)||((List.length lst2)=0)) then
        []
    else begin
        pair((List.hd lst1),lst2)@cross((List.tl lst1),lst2)
    end
    ;;

let compl (dfa) =
    {states=dfa.states;
     alphabet=dfa.alphabet;
     start=dfa.start;
     delta=dfa.delta;
     final=difference(dfa.states,dfa.final)}
    ;;

let rec twoElementTupleToString (first,last) =
    "("^first^","^last^")"

let rec listOfTuplesToStrings (lst) =
if ((List.length lst)=0) then
        []
    else
        let first,last = (List.hd lst) in
            twoElementTupleToString(first,last)::
            listOfTuplesToStrings(List.tl lst)

let rec generateDelta (trans,d1,d2) =
    if ((List.length trans)=0) then
        []
    else
        let ((oldState1,oldState2),input)=(List.hd trans) in
            (twoElementTupleToString(oldState1,oldState2),input,
                twoElementTupleToString(
                    transition(d1,oldState1,input),
                    transition(d2,oldState2,input)))::
                generateDelta((List.tl trans),d1,d2)
    ;;

let inter (d1,d2) =
    let newstates=cross(d1.states,d2.states) in
        {states=listOfTuplesToStrings(newstates);
         alphabet=d1.alphabet;
         start=twoElementTupleToString(d1.start,d2.start);
         delta=generateDelta(cross(newstates,d1.alphabet),d1,d2);
         final=listOfTuplesToStrings(cross(d1.final,d2.final))}
    ;;

let union (d1,d2) =
    let newstates=cross(d1.states,d2.states) in
        {states=listOfTuplesToStrings(newstates);
         alphabet=d1.alphabet;
         start=twoElementTupleToString(d1.start,d2.start);
         delta=generateDelta(cross(newstates,d1.alphabet),d1,d2);
         final=listOfTuplesToStrings(
            cross(d1.final,d2.states)@
            cross(d1.states,d2.final))}
    ;;