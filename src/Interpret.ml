open Language

(* Interpreter for expressions *)
module Expr =
  struct

    open Expr

    let of_binop op =
		let bti = function true -> 1 | _ -> 0 in
		let (&) f g = fun x y -> f (g x y) in 
		List.assoc op
			[ "+", ( + );
			  "-", ( - );
			  "*", ( * );
			  "/", ( / );
			  "%", ( mod );
			  "<",  bti & (<);
			  "<=", bti & (<=);
			  ">",  bti & (>);
			  ">=", bti & (>=);
			  "==", bti & (==);
			  "!=", bti & (<>);
			  "&&", (fun x y -> if x <> 0 && y <> 0 then 1 else 0);
			  "!!", (fun x y -> if x <> 0 || y <> 0 then 1 else 0)
			]


    let rec eval expr st = 
      let eval' e = eval e st in
      match expr with
      | Var   x     -> st x
      | Const z     -> z
      | Binop (op, x, y)   -> of_binop op (eval' x) (eval' y)

  end

(* Interpreter for statements *)
module Stmt =
  struct

    open Stmt

    (* State update primitive *) 
    let update st x v = fun y -> if y = x then v else st y 
      
    let rec eval stmt ((st, input, output) as conf) =
      match stmt with
      | Skip           -> conf
      | Assign (x, e)  -> (update st x (Expr.eval e st), input, output)
      | Read    x      -> 
			let z :: input' = input in 
            (update st x z, input', output)
      | Write   e      -> (st, input, output @ [Expr.eval e st])
      | If (e, s1, s2) -> if (Expr.eval e st) <> 0 then (eval s1 conf)
												   else (eval s2 conf)
	  | While (e, s)   -> if (Expr.eval e st) <> 0 then eval stmt (eval s conf)
												   else conf	
	  | Until (seq, exp)   ->  
			let rec loop exp' seq' conf' = 
            let (newst, _, _) as newconf = eval seq' conf' in
				if (Expr.eval exp' newst) == 0
				then loop exp' seq' newconf
				else newconf
			in loop exp seq conf
							
	  (*let (new_st, _, _) as new_conf = eval stmt (eval s conf) in 
	    if (Expr.eval e st) == 0 then eval stmt (eval s new_conf) 
	                         	 else new_conf	*)
	                         	 							  										  
      | Seq (s1, s2)   -> eval s1 conf |> eval s2

  end

module Program =
  struct

    let eval p input = 
      let (_, _, output) = 
	Stmt.eval p ((fun _ -> failwith "undefined variable"), input, []) 
      in
      output

  end
