(* Derek Redfern
 * dredfern.olin@gmail.com
 * FOCS Homework 0
 * 9.16.13
 *
 * Testing variables defined at bottom; uncomment at will
 *)

open List;;

let append (a,b) = 
	a@b;;

let rec flatten (a) =
        if ((List.length a) == 0) then
		[]
	else if ((List.length a) == 1) then
                List.hd a
        else
                (List.hd a) @ flatten(List.tl a)
        ;;

let rec double (a) =
	if ((List.length a) == 0) then
		[]
	else if ((List.length a) == 1) then
		[(List.hd a)*2]
	else
		[(List.hd a)*2] @ double(List.tl a)
	;;

let rec last (a) =
	if ((List.length a) == 0) then
		None
	else if ((List.length a) == 1) then
		Some (List.hd a)
	else
		last(List.tl a)
	;;

type rat = {num: int; den: int};;

let rec gcd (a,b) =
	if (b = 0) then
		a
	else
		gcd(b, a mod b)
	;;

let simplify (a) =
	let divide = gcd(a.num,a.den) in
	{num = a.num/divide; den = a.den/divide}
	;;

let addR (a,b) =
	let newa = {num = a.num*b.den; den = a.den*b.den} in
	let newb = {num = b.num*a.den; den = b.den*a.den} in
	simplify({num = (newa.num + newb.num); den = newa.den})
	;;

let multR (a, b) =
	let result = {num = a.num*b.num; den = a.den*b.den} in
	simplify(result)
	;;

type number = I of int | R of rat | F of float;;

let rat_of_int (a : int) =
	{num = a; den = 1}
	;;

let float_of_rat (a : rat) =
	(float a.num) /. (float a.den)
	;;

let add (a,b) =
	match (a,b) with
                I a, I b -> I (a + b)
                | R a, R b -> R (addR(a,b))
                | F a, F b -> F (a +. b)
				| I a, R b -> R (addR((rat_of_int a),b))
                | R a, I b -> R (addR(a,(rat_of_int b)))
                | I a, F b -> F ((float a) +. b)
                | F a, I b -> F (a +. (float b))
                | R a, F b -> F ((float_of_rat a) +. b)
                | F a, R b -> F (a +. (float_of_rat b))
	;;

type bConst = One | Zero;;
type bExpr = Const of bConst
				| Var of string
				| And of bExpr * bExpr
				| Or of bExpr * bExpr
				| Not of bExpr;;

let rec vars (a) =
	match (a) with
			Const x -> []
			| Var x -> [x]
			| And (x,y) -> vars(x)@vars(y)
			| Or (x,y) -> vars(x)@vars(y)
			| Not x -> vars(x);;

let rec subst (b,v,s) =
	match (b:bExpr) with
			Const x -> b
			| Var x -> if (Var x)=(Var v) then s else (Var x)
			| And (x,y) -> And(subst(x,v,s),subst(y,v,s))
			| Or (x,y) -> Or(subst(x,v,s),subst(y,v,s))
			| Not x -> Not(subst(x,v,s));;

let rec eval (b) =
	match (b:bExpr) with
			Const x -> Some(x)
			| Var x -> None
			| And (x,y) -> let val1=eval(x) in
							let val2=eval(y) in
								if (val1=None or val2=None)
									then None
								else if (val1=(Some One) && val2=(Some One))
									then (Some One) 
									else (Some Zero)
			| Or (x,y) -> let val1=eval(x) in
							let val2=eval(y) in
								if (val1=None || val2=None)
									then None
								else if (val1=(Some One) or val2=(Some One)) 
									then (Some One) 
									else (Some Zero)
			| Not x -> let val1=eval(x) in
							if val1=None 
								then None
							else if val1=(Some One) 
								then (Some Zero) 
								else (Some One)
			;;

(*
let half = R {num=1;den=2};;
let third = R {num=1;den=3};;
let fourth = R {num=1;den=4};;
let sample1 = And(Not(Var "a"), Not(Var "b"));;
let sample2 = Or(Not(Var "a"), And(Var "b", Const(One)));;
let sample3 = And(Var "a", Not(Var "a"));;
let sample1' = subst(sample1, "a", Const One);;
let sample1'' = subst(sample1', "b", Const Zero);;
*)