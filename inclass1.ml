let rec mapAppend f xs =
	match xs with
		[] -> []
		| x::xs' -> (f x) @ (mapAppend f xs')
;;

let flatten lst =
	mapAppend (fun x -> x) lst
;;

let cross (lst1, lst2) =
	mapAppend(fun x -> mapAppend (fun y -> [(x,y)]) lst2) lst1
;;

let rec genMap f1 f2 xs =
	match xs with
		[] -> []
		| x::xs' -> (f1 (f2 x) (genMap f1 f2 xs'))
;;

let rec genMap2 comb xs =
	match xs with
		[] -> []
		| x::xs' -> comb x (genMap2 comb xs')

(* let rec genTransform op arg1 arg2 =
	match lst with
		[] -> []
		| o *)

let rec fold_right comb xs base =
    match xs with
        [] -> base
        | x :: xs' -> comb x (fold_right comb xs' base)

let sum xs = fold_right (fun x r -> x+r) xs 0

let last lst = fold_right (fun x r -> match r with None -> Some x | _ -> r) lst None