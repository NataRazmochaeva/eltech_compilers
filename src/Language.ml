(* AST for expressions *)
module Expr =
  struct

    type t =
    | Var   of string
    | Const of int
    | Add   of t * t
    | Mul   of t * t 
    | Binop of string * t * t

    ostap (
      parse: expr0;
      expr0: h:expr1 t:(-"!!" expr1)* {
			List.fold_left(fun e op -> Binop("!!", e, op)) h t};
      expr1: h:expr2 t:(-"&&" expr2)* {
			List.fold_left(fun e op -> Binop("&&", e, op)) h t};
      expr2: h:expr3 t:(("==" | "!=" | "<=" | "<" | ">=" | ">") expr3)? {
			match t with
				| None 		   -> h
				| Some (op, y) -> Binop(Ostap.Matcher.Token.repr op, h, y)};
      expr3: h:expr4 t:(("+" | "-") expr4)* {
			List.fold_left(fun e (op, y) -> Binop(Ostap.Matcher.Token.repr op, e, y)) h t};
      expr4: h:prim t:(("*" | "/" | "%") prim)* {
			List.fold_left(fun e (op, y) -> Binop(Ostap.Matcher.Token.repr op, e, y)) h t};
      prim:
       n:DECIMAL	{Const n}
       | e:IDENT 	{Var e}
       | -"(" parse -")"
       
    )

  end

(* AST statements/commands *)
module Stmt =
  struct

    type t =
    | Skip
    | Assign of string * Expr.t
    | Read   of string
    | Write  of Expr.t
    | Seq    of t * t
    | If     of Expr.t * t * t
    | While  of Expr.t * t
    | Until  of t * Expr.t
    
    let expr = Expr.parse

    ostap (
      statement:
        x:IDENT ":=" e:expr  	   {Assign (x, e)}
        | %"read"  "(" x:IDENT ")" {Read x}
        | %"write" "(" e:expr  ")" {Write e}
        | %"skip"                  {Skip}
        | %"if" e:expr %"then" s1:sequence
          s2:elsePart? 
          %"fi" {If (e, s1, match s2 with None -> Skip | Some s2 -> s2)}
		| %"while" e:expr
		  %"do"	   s:sequence 
		  %"od"	{While (e, s)}
		| %"for" s1:sequence "," e:expr	"," s2:sequence	
		  %"do" s:sequence 
		  %"od" {Seq (s1, While (e, Seq(s, s2)))}
		| %"repeat" s:sequence
		  %"until" e:expr {Until (s, e)};
      elsePart:
          %"else" sequence
        | %"elif" e:expr  %"then" s1:sequence 
          s2:elsePart?
          {If (e, s1, match s2 with None -> Skip | Some s2 -> s2)};
      sequence:
        s:statement ";" d:sequence {Seq (s, d)}
        | statement
    )

  end

module Program =
  struct

    type t = Stmt.t

    let parse = Stmt.sequence

  end

