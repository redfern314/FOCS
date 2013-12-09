let double x = 2*x;;

let rec doubles (lst) =
	match lst with
		[] -> []
		| head :: tail -> (double head) :: doubles(tail)
;;

let repl x = (x,x);;

let rec diags (lst) =
	match lst with
		[] -> []
		| head :: tail -> (repl head) :: diags(tail)
;;

let opt x = match x with
				| [] -> None
				| head :: _ -> Some head

let rec heads (lstlst) =
	match lstlst with
		[] -> []
		| head :: tail -> opt head :: heads(tail)
;;

(* let triptransform (x) = (x,x+1,x+2);; *)

(* let triples (lst) = map(triptransform,x);; *)

let pairs (item,lst) = map((fun x -> (fun a -> (x,a)))item)lst;;

let add m n = m + n;;
let mult m n = m * n;;

let compose f1 f2 = fun i -> (f1 (f2 i));;