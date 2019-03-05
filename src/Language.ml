(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let from_bool_to_int b = if b then 1 else 0

    let from_int_to_bool i= i!= 0

    let get_oper op l_e r_e = match op with
	    |"+" -> l_e + r_e
	    |"-" -> l_e - r_e
	    |"*" -> l_e * r_e
	    |"/" -> l_e / r_e
	    |"%" -> l_e mod r_e
	    |">" -> from_bool_to_int (l_e > r_e)
	    |"<" -> from_bool_to_int (l_e < r_e)
	    |">=" -> from_bool_to_int (l_e >= r_e)
	    |"<=" -> from_bool_to_int (l_e <= r_e)
	    |"==" -> from_bool_to_int (l_e == r_e)
	    |"!=" -> from_bool_to_int (l_e != r_e)
	    |"!!" -> from_bool_to_int(from_int_to_bool l_e || from_int_to_bool r_e)
	    |"&&" -> from_bool_to_int(from_int_to_bool l_e && from_int_to_bool r_e)

    let rec eval s expres = match expres with
	    |Const c -> c 
	    |Var v -> s v
	    |Binop (op,l_e,r_e) -> get_oper op (eval s l_e) (eval s r_e)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    let do_Bin op =  ostap(- $(op)), (fun x y -> Binop (op, x, y))
    
    ostap (
                  expr:
			!(Ostap.Util.expr
				(fun x -> x)
				(Array.map (fun (a, ops) -> a, List.map do_Bin ops)
				[|
				`Lefta, ["!!"];
                  		`Lefta, ["&&"];
                  		`Nona , ["=="; "!="; "<="; ">="; "<"; ">"];
                  		`Lefta, ["+"; "-"];
                  		`Lefta, ["*"; "/"; "%"];
				|]
				primary
			);
		primary: x:IDENT {Var x} | c:DECIMAL {Const c} | -"(" expr -")"
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (s, i, o) p = match p with
    | Read variable_name  -> (Expr.update variable_name  (hd i) s, tl i, o)
    | Write expression   -> (s, i, o @ [Expr.eval s expression])
    | Assign (variable_name, expression  ) -> (Expr.update variable_name (Expr.eval s expression ) s, i, o)
    | Seq (e1, e2)  -> eval (eval (s, i, o) e1) e2;;  

    (* Statement parser *)
    ostap (
      stmt:
			    x:IDENT ":=" e:!(Expr.expr) {Assign(x, e)}
			    | "read" "(" x:IDENT ")" {Read x}
			    | "write" "(" e:!(Expr.expr) ")" {Write e};
			
		  parse: s:stmt ";" rest:parse {Seq(s, rest)} | stmt
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
