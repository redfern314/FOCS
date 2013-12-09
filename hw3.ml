(* Derek Redfern
 * dredfern.olin@gmail.com
 * FOCS Homework 3
 * 10.9.13
 *
 * Collaboration: [none]
 *
 *)



(* 
 * CODE FOR HOMEWORK 3
 *
 *)


let fail str = raise (Failure str)




(*************************************************************
 *
 * QUESTION 2
 *
 *************************************************************)


(*
 *  Definition of a binary tree
 *
 *)

type 'a bintree = 
    EmptyTree 
  | Node of 'a * 'a bintree * 'a bintree


(*
 * Sample tree used in the homework write-up
 *
 *)

let sample = 
  Node (10, Node (3, Node (7, EmptyTree, EmptyTree),
                     Node (5, EmptyTree, EmptyTree)),
            Node (6, Node (99, EmptyTree, 
                               Node (66, EmptyTree, EmptyTree)),
                     EmptyTree))


(*
 * Size and sum functions as examples
 *
 *)

let rec size t =
  match t with
    EmptyTree -> 0
  | Node (_,l,r) -> 1 + (size l) + (size r)

let rec sum t = 
  match t with
    EmptyTree -> 0
  | Node (v,l,r) -> v + (sum l) + (sum r)


(*
 * Mapping and folding functions for binary trees
 *
 *)

let rec mapT f t = 
  match t with
    EmptyTree -> EmptyTree
  | Node (v,l,r) -> Node (f v, mapT f l, mapT f r)

let rec foldT comb t b = 
  match t with
    EmptyTree -> b
  | Node (v,l,r) -> comb v (foldT comb l b) (foldT comb r b)


(*
 * Overworked code for printing binary trees
 *
 *)

let print_tree t = 
  let emptyString n = String.make n ' '  in
  let ljustify n s = s ^ (emptyString (n - (String.length s)))  in
  let height p = List.length p  in
  let width p = List.fold_right (fun s m -> max (String.length s) m) p 0  in
  let rec copy n x = 
    if (n <= 0)
      then []
    else x :: copy (n - 1) x  in
  let empty h w = copy h (emptyString w)  in
  let above p q = 
    let w = max (width p) (width q)
    in (List.map (ljustify w) p) @ (List.map (ljustify w) q)  in
  let beside p q = 
    let h = max (height p) (height q)  in
    let heighten h p = above p (empty (h - List.length p) (width p))
    in List.map2 (^) (heighten h p) (heighten h q)  in
  let string_picture p = (String.concat "\n" p)^"\n"  in
  let print_picture p = Printf.printf "%s" (string_picture p)  in
  let rec picture_tree f t = 
    match t with
      EmptyTree -> [" "]
    | Node (v,EmptyTree,EmptyTree) -> [f v]
    | Node (v,EmptyTree,r) -> above [f v]
          (above ["---|"]
             (beside ["   "] (picture_tree f r)))
    | Node (v,l,EmptyTree) -> above [f v]
          (above ["|"] 
             (picture_tree f l))
    | Node (v,l,r) -> let sub_l = picture_tree f l in
      let sub_r = picture_tree f r
      in above [f v]
        (above ["|"^(String.make (2 + width sub_l) '-')^"|"]
           (beside sub_l (beside ["   "] sub_r)))
  in print_picture (picture_tree string_of_int t)



(*************************************************************
 *
 * QUESTION 4
 * 
 *************************************************************)


(* 
 * String to Characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode str = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode cs = 
  let str = String.create(List.length(cs))
  in (List.iteri (fun i c -> String.set str i c) cs; str)


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
 * Accept function for a DFA
 *
 *)

let accept dfa input = 
  let isFinal state = List.mem state dfa.final  in
  let rec transition state sym delta =  match delta with
      [] -> fail (Printf.sprintf "cannot transition on input %c" sym)
    | (q1,c,q2) :: delta' -> if (q1=state && c=sym) then q2
                             else transition state sym delta'  in
  let rec extTransition q cs = match cs with
      [] -> q
    | c :: cs' -> extTransition (transition q c dfa.delta) cs'
  in isFinal (extTransition dfa.start (explode input))


(* 
 *  Compute the language of a DFA, restricted to inputs of length <= n
 *   strings alphabet n: returns all strings of length <= n over alphabet
 *   language dfa n: returns a list of strings accepted by dfa
 *   printLanguage dfa n: prints the strings accepted by dfa
 *
 *)

let strings alphabet n = 
  let rec range n m = if (n > m) then [] else n::(range (n+1) m)  in
  let mapCons l c = List.map (fun s -> (c::s)) l  in
  let makeStrs n ss = []::(List.concat (List.map (mapCons ss) alphabet))  in
  let strings' r = List.fold_right makeStrs r [[]] 
  in List.map implode (strings' (range 1 n))

let language dfa n = 
  List.filter (accept dfa) (strings dfa.alphabet n)

let printLanguage dfa n = 
  List.iter 
    (fun s -> Printf.printf "   %s\n" (if (s="") then "<empty>" else s))
    (language dfa n)




(*
 * A little set library
 *
 * only the functions described at the top of the module
 *  (between 'sig' and 'end') are available
 *
 *)


module Set : sig
  type 'a set

  val empty : 'a set
  val make : 'a list -> 'a set
  val union : 'a set -> 'a set -> 'a set
  val inter : 'a set -> 'a set -> 'a set
  val is_member : 'a -> 'a set -> bool
  val is_empty : 'a set -> bool
  val is_subset : 'a set -> 'a set -> bool
  val list_of_subsets : 'a set -> 'a set list
  val add : 'a -> 'a set -> 'a set

  val fold : ('a -> 'b -> 'b) -> 'a set -> 'b -> 'b

  val print_string_set : Format.formatter -> string set -> unit

end = struct
  type 'a set = 'a list

  module L = List

  let remove_dups xs = 
    L.fold_right 
     (fun x xs' -> if (L.mem x xs') then xs' else x::xs')
       xs []    

  let make xs = List.sort compare (remove_dups xs)

  let empty = make []

  let is_empty xs = (xs=[])

  let is_member x xs = L.mem x xs

  let union xs ys = make (xs @ ys)

  let inter xs ys = 
    L.fold_right
     (fun x xs' -> if (L.mem x ys) then x::xs' else xs')
      xs []

  let is_subset xs ys = 
    L.fold_right
     (fun x rest -> (L.mem x ys) && rest)
       xs true

  let add x xs = make (x :: xs)

  let list_of_subsets xs = 
    L.fold_right
     (fun x xss' -> xss' @ (L.map (add x) xss'))
      xs [[]]

  let fold f xs v = L.fold_right f xs v

  let map f xs = L.fold_right (fun x xs' -> add (f x) xs') xs empty

  let print_string_set ppf ss = 
    let ss' = List.map (fun s -> "\""^s^"\"") ss
    in Format.fprintf ppf "{%s}" (String.concat "," ss')

end

(* Hack to get sets of strings to display in the OCaml shell
 * so that we can eyeball results of subsetConstruction *)

#install_printer Set.print_string_set



(* 
 * The type for an NFA, parameterized by the type for the states 
 *
 *)

type 'a nfa = {states_N :   'a list;
               alphabet_N : char list;
               start_N :    'a list;
           delta_N :    ('a * char * 'a list) list;
         final_N :    'a list}


(*
 * Some sample NFAs
 *
 *)

let abaabStar = {states_N = ["s";"a11";"a21";"a22"];
                 alphabet_N = ['a';'b'];
                 start_N = ["s"];
                 delta_N = [("s", 'a', ["a11"; "a21"]);
                            ("a21", 'a', ["a22"]);
                            ("a11", 'b', ["s"]);
                            ("a22", 'b', ["s"])];
                 final_N = ["s"]}

let abaab = {states_N = ["q1"; "q2"; "q3"; "q4"; "q5"];
             alphabet_N = ['a'; 'b'];
             start_N = ["q1"];
             delta_N = [("q1", 'a', ["q2"; "q3"]);
                        ("q2", 'b', ["q5"]);
                        ("q3", 'a', ["q4"]);
                        ("q4", 'b', ["q5"])];
             final_N = ["q5"]}

(* My custom code for Homework 3 is below! *)

(* Question 1 *)

let atLeast num p lst = (List.length (List.filter p lst)) >= num;;
let maxL lst =
  List.fold_right 
    (fun x r -> match r with 
      None -> (Some x) | Some (v : int) -> if (v>x) then (Some v) else (Some x)) lst None;;

let mapFuns flst x = List.map (fun f -> f x) flst;;

let mapCross flst xlst = List.flatten (List.map (mapFuns flst) xlst);;

(* Question 2 *)

let rec height tree =
  match tree with
    EmptyTree -> 0
    | Node (v,l,r) -> 1 + (max (height l) (height r))
;;

let height' tree = foldT (fun v r1 r2 -> 1 + (max r1 r2)) tree 0;;

let rec fringe tree =
  match tree with
    EmptyTree -> []
    | Node (v,l,r) -> let fr = (fringe r) and fl = (fringe l) in
        if ((fr=[])&&(fl=[])) then
          v::fl@fr
        else
          fl@fr
;;

let fringe' tree = foldT 
  (fun v r1 r2 -> if ((r1=[])&&(r2=[])) then v::r1@r2 else r1@r2)
  tree [];;

let preorder tree = foldT
  (fun v r1 r2 -> v::(r1@r2))
  tree [];;

let postorder tree = foldT
  (fun v r1 r2 -> (r1@r2)@[v])
  tree [];;

let inorder tree = foldT
  (fun v r1 r2 -> (r1@[v])@r2)
  tree [];;

let split lst = List.fold_right
  (fun x r -> if (List.length (fst r))<(List.length (snd r)) then (x::fst(r),snd(r)) else (fst(r),x::snd(r))) 
  lst ([],[]);;

let rec makeTree lst =
  match lst with
    [] -> EmptyTree
    | x::xs -> let slst = (split xs) in Node (x,(makeTree (fst slst)),(makeTree (snd slst)))
;;

(* Question 3 *)

let suffixes lst = List.fold_right
  (fun x r -> (x::(List.hd r))::r)
  lst [[]];;

let prefixes lst = List.fold_left
  (fun r x -> ((List.hd r)@[x])::r)
  [[]] lst;;

let inject item lst =
  List.map2 (fun a b -> (a@[item])@b) (prefixes lst) (List.rev (suffixes lst));;

let perms lst =
  List.fold_right
  (fun x r -> (List.flatten (List.map (inject x) r)))
  lst [[]];;