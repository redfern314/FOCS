(* Derek Redfern
 * dredfern.olin@gmail.com
 * FOCS Homework 5
 * 10.25.13
 *
 * Collaboration: [none]
 *
 *)


(* 
 * CODE FOR HOMEWORK 5
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
    else loop (index-1) ((String.get str index)::result)
  in
    loop (String.length str - 1) []

let implode (cs) = 
  let str = String.create(List.length(cs))
  in (List.iteri (String.set str) cs; str)



(*
 * A function to perform depth first search in a tree
 * created from a transition relation.
 *
 * dfs quit trans init goal   
 *       returns true if there is a node N in the 
 *       tree created by the transition relation 'trans' starting
 *       at the initial node 'init' such that 'goal N' is true.
 *
 *       Quit search the current branch when 'quit N' is true 
 *
 * The 'quit' function is used to control the depth of the search. 
 * When 'quit N' returns true, it means that we have gone deep enough
 * and that we're acknowledging that we won't find the solution on
 * that branch.   
 *
 *)

let dfs quit trans init goal = 
  let rec loop seen stack = 
    match stack with
      [] -> false
    | curr :: stack' when not (List.mem curr seen) -> 
           if (goal curr) then true
       else if (quit curr) then loop (curr::seen) stack'
       else loop (curr::seen) ((trans curr)@stack')
    | _:: stack' -> loop seen stack'
  in loop [] [init]



(*
 * Some types to help us define CFGs (without epsilon production rules)
 *
 * symbol is either a terminal 'T char' or a nonterminal 'N string'
 *    (I'm using strings to describe nonterminals for added flexibility
 *
 * rule is the type of a non-epsilon production rule
 *    A rule can only be constructed with mk_rule that takes the 
 *    the nonterminal name 'A' and the list of symbols to produce 'w',
 *    and creates the rule 'A -> w', and it can only be used using
 *    get_rule that returns the components 'A' and 'w' of 'A -> w'
 *
 *)

type symbol = 
    T of char 
  | N of string


(* this module is simply to hide the implementation of the rule type 
 * so that the only way to create a rule is to use mk_rule below *)

module Rule : sig 
  type t
  val mk: string -> symbol list -> t
  val get: t -> string * symbol list 
  val strSymL : symbol list -> string
  val print_rule: Format.formatter -> t -> unit
end = struct
  type t = string * symbol list
  let mk s sl =
    match sl with
    | [] -> fail "Trying to create epsilon rule!"
    | _ -> (s,sl)
  let get r = r
  let strS (s) = "\""^s^"\""
  let strSym = function 
    | (T c) -> "T \'"^(Char.escaped c)^"\'"
    | (N s) -> "N "^(strS s)  
  let strSymL sl = "["^(String.concat "; " (List.map strSym sl))^"]"
  let print_rule ppf (s,sl) = 
    Format.fprintf ppf "(%s,%s)" (strS s) (strSymL sl)
end
;; #install_printer Rule.print_rule


type rule = Rule.t

(* this is used to create a non-epsilon production rule 
 * it fails if the symbol list is empty
 *)

let mk_rule : string -> symbol list -> rule = Rule.mk

(* this extracts the components of a production rule
 *)

let get_rule : rule -> string * symbol list = Rule.get




(*
 * The type for CFGs with no epsilon production rules
 *
 *)

type cfgNE = { nonterms_ne     : string list;
                terms_ne        : char list;
                rules_ne       : rule list;
                gen_empty_ne   : bool;
        start_ne       : string }



(* function to test if a symbol list is the sequence of terminals we seek *)

let found w = 
  let w' = List.map (fun c -> T c) (explode w)  in
  fun str -> str = w'


(* ------------ My code starts here ------------ *)

(* A predicate for filtering the different types of symbols. *)
let isNonterminal sym =
  match sym with
    T _ -> false
    | N _ -> true
;;

(* Gives the result when a rule is applied to a specific nonterminal symbol
   in a list. *)
let applyRuleToSym syms i nt rule =
  let (nt_r,syms_r) = (get_rule rule) in
    (List.flatten (List.mapi (fun newi x -> if (i=newi) then syms_r else [x]) syms))
;;

(* A predicate for filtering the rules in a CFG to only those applicable to
 * the current nonterminal symbol. *)
let matchRule nt rule =
  let (nt_r,syms) = (get_rule rule) in 
    match nt_r,nt with
      _,N nt_n -> nt_r=nt_n
      | _,_ -> false
;;

(* Outputs all possible steps, given a CFG and a specific nonterminal symbol
   from an ordered list of symbols. *)
let stepOne cfg syms i nt = 
  if (isNonterminal nt) then
    (List.map (applyRuleToSym syms i nt) (List.filter (matchRule nt) cfg.rules_ne))
  else
    []
;;

(* Outputs all possible steps, given a CFG and list of symbols. *)
let step cfg syms = (List.flatten (List.mapi (stepOne cfg syms) syms));;

(* ------------- My code ends here ------------- *)


(* Function to search the derivation tree for a sequence of terminals
 * matching the provided string 
 * It searches until the sequence of symbols grows larger than the
 * string looked for.
 *)

let generate cfgNE w = 
  if (w = "") then cfgNE.gen_empty_ne
  else dfs (fun syms -> List.length syms > String.length w)
           (step cfgNE)
           [N cfgNE.start_ne]
           (found w)



(* 
 *  Compute the language of a CFG, restricted to inputs of length <= n
 *   language(cfg,n) returns a list of strings generated by cfg
 *   printLanguage(cfg,n) prints the strings generated by cfg
 *
 *)

let language cfgNE n = 
  let strings alphabet n = 
    let rec mapCons c = List.map (fun y -> c::y)  in
    let rec mapConsSet alphabet l = 
      List.fold_right (fun c -> List.append (mapCons c l)) alphabet []  in
    let rec strings' n =
      if (n<=0) then [[]]
      else [] :: mapConsSet alphabet (strings' (n-1))
    in List.map implode (strings' n)  in
  List.filter (generate cfgNE) (strings cfgNE.terms_ne n)

let printLanguage cfgNE n = 
  List.iter (fun s -> Printf.printf "  %s\n" (if (s="") then "<empty>" else s))
              (language cfgNE n)




(* 
 *  Some sample CFGs without any epsilon production rules
 *
 *)


(* The language {a^nb^n | n>=0}   *)

let anbn = { nonterms_ne = ["start"];
             terms_ne = ['a';'b'];
             rules_ne = [mk_rule "start" [T 'a'; T 'b'];
                         mk_rule "start" [T 'a'; N "start"; T 'b']];
             gen_empty_ne = true;
             start_ne = "start" }



(* The language {a^nb^nc^m | m>=0}   *)

let anbncm = { nonterms_ne = ["start"; "A"; "B"];
               terms_ne = ['a'; 'b'; 'c'];
           rules_ne = [ mk_rule "start" [N "A"];
                mk_rule "start" [N "B"];
                mk_rule "start" [N "A"; N "B"];
                mk_rule "A" [T 'a'; T 'b'];
                mk_rule "A" [T 'a'; N "A"; T 'b'];
                mk_rule "B" [T 'c'];
                mk_rule "B" [T 'c'; N "B"]];
           gen_empty_ne = true;
           start_ne = "start" }




(*
 * The type for general CFGs 
 *
 *)

type cfg = { nonterms : string list;
         terms    : char list;
         rules    : (string * symbol list) list;
         start    : string }



(* A sample grammar with language {a^nb^nb^ma^mc^p | m,n,p>=0}  *)

let anbnbmamcp = { nonterms = ["Start"; "A"; "B"; "C"];
           terms = ['a';'b';'c'];
           rules = [ ("Start", [N "A"; N "B"; N "C"]);
                 ("A", [T 'a'; N "A"; T 'b']);
                 ("A", []);
                 ("B", [T 'b'; N "B"; T 'a']);
                 ("B", []);
                 ("C", [T 'c'; N "C"]);
                 ("C", [])];
           start = "Start" }

let aba = { nonterms = ["Start"; "A"; "C"];
           terms = ['a';'b';'c'];
           rules = [ ("Start", [N "A"; N "C"; N "A"]);
                 ("A", [T 'a'; N "A"; T 'b']);
                 ("A", []);
                 ("C", [T 'c'; N "C"]);
                 ("C", [])];
           start = "Start" }

let sneaky = { nonterms = ["Start"; "A"; "B"; "C"];
           terms = ['a';'b';'c'];
           rules = [ ("Start", [N "B"; N "C"]);
                 ("A", [T 'a'; N "A"]);
                 ("A", []);
                 ("B", [T 'b'; N "B"]);
                 ("B", [N "A"]);
                 ("C", [T 'c'; N "C"]);
                 ("C", [])];
           start = "Start" }
                   

(* ------------ My code starts here ------------ *)

(* NOTE: I was not able to complete this problem quite to my satisfaction.
   Running my code on sneaky (above) fails, as it does not catch the NEW
   epsilon rule that is created. With more time, I would've been able to
   remedy this problem. *)

(* Pretty self-explanatory. Support predicates. *)
let isEpTrans (nt,syms) = syms=[];;
let isNotEpTrans (nt,syms) = syms!=[];;

(* This implementation of contains returns the boolean result, the length
 * of the list, and a list of all the indices of occurances of the relevant 
 * item in the list (all wrapped into a tuple). *)
let contains lst item = List.fold_left
  (fun (r,i,ilst) x -> if x=item then (true,i+1,i::ilst) else (r,i+1,ilst))
  (false,0,[]) lst;;

(* To stop OCaml being ticked off about my type mismatches... *)
let ntToString x =
  match x with
    N xnt -> xnt
    | T xt -> (Char.escaped xt)
;;

(* Takes in one nonterminal character representing an epsilon transition 
 * rule (ep), one rule from the original list (nt,syms), and the new list so 
 * far (r) [as well as whether any previous iterations of this function have
 * added to it]. Returns r with a replacement rule appended for every occurence
 * of the given nonterminal character in syms. 
 *
 * This function is necessary for grammars containing rules such as S -> ABA
 * (run (eliminate_epsilon_rules aba) for an example)
 *)
let eliminateOneEpOneRuleOneInstance nt syms i (appended,r) =
  let newrule = (nt,(List.flatten (List.mapi (fun newi x -> if (i=newi) then [] else [x]) syms))) in
    let (res,_,_) = (contains r newrule) in
    if res then
      (appended,r)
    else
      (true,newrule::r)
;;

(* Takes in one nonterminal character representing an epsilon transition 
 * rule (ep), one rule from the original list (nt,syms), and the new list so 
 * far (r) [as well as whether any previous iterations of this function have
 * added to it]. Returns r with a replacement rule appended for every occurence
 * of the given nonterminal character in syms. *)
let eliminateOneEpOneRule ep (nt,syms) (appended,r) = 
  let (res,i,ilst) = (contains syms (N ep)) in
    if res then
      let (newappended,newr) = (List.fold_right (eliminateOneEpOneRuleOneInstance nt syms) ilst (appended,r)) in
        (newappended,newr)
    else
      (appended,r)
;;

(* Takes in one nonterminal character representing an epsilon transition
 * rule (x) and the list of rules (r). Eliminates the given rule from 
 * the list of rules by adding replacement non-ep-transition
 * rules to r and returning for the next item in fold_right. 
 *
 *)
let rec eliminateOneEp x (appended,r) = 
  (List.fold_right (eliminateOneEpOneRule x) r (appended,r))
;;

(* This function takes in a set of rules and continually replaces all the
 * epsilon transitions until the only one left is (optionally) the
 * top-level transition.
 *
 * Note: This is my only explicitly recursive function. I'm not sure how
 * to implement such a loop without explicitly recursing. Sorry!
 *
 * This loop is necessary for grammars where removing one epsilon creates
 * another: for instance, a grammar where A -> ep, B -> A. Run
 * (eliminate_epsilon_rules sneaky) for an example. *)
let rec eliminate_recurse r eps i =
  let (appended,newr) = (List.fold_right 
    eliminateOneEp 
    (List.map (fun (nt,syms) -> nt) eps)
    (false,(List.filter isNotEpTrans r))) in
      if (appended && i<20) then
        (eliminate_recurse newr eps (i+1))
      else
        r
;;

(* Removes a rule of the form S -> ep (if one exists). *)
let eliminateStartEp startEp ilst r =
  if startEp then
    (List.flatten (List.mapi (fun newi x -> if ((List.hd ilst)=newi) then [] else [x]) r))
  else
    r
;;

let eliminate_epsilon_rules cfg = 
  let rules = (eliminate_recurse cfg.rules (List.filter isEpTrans cfg.rules) 0) in
    let (gen_empty_ne,_,ilst) = (contains rules (cfg.start,[])) in
      let rules_ne = (List.map (fun (nt,syms) -> (mk_rule nt syms)) (eliminateStartEp gen_empty_ne ilst rules)) in
        {nonterms_ne = cfg.nonterms;
          terms_ne = cfg.terms;
          rules_ne = rules_ne;
          gen_empty_ne = gen_empty_ne;
          start_ne = cfg.start}
;;

(* ------------- My code ends here ------------- *)