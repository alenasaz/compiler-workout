open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval cfg prg =
  let step (st, (s, i, o)) p = match p with
    | BINOP op -> (Language.Expr.get_oper op (hd (tl st)) (hd st) :: (tl (tl st)), (s, i, o))
    | CONST n  -> (n :: st, (s, i, o))
    | READ     -> (hd i :: st, (s, tl i, o))
    | WRITE    -> (tl st, (s, i, o @ [hd st]))
    | LD variable_name    -> (s variable_name :: st, (s, i, o))
    | ST variable_name    -> (tl st, (Language.Expr.update variable_name (hd st) s, i, o))
  in match prg with
    | [] -> cfg
    | p :: ps -> eval (step cfg p) ps

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile_expr e = match e with
    | Language.Expr.Const n -> [CONST n]
    | Language.Expr.Var v -> [LD v]
    | Language.Expr.Binop (op, l_e,r_e) -> compile_expr l_e@ compile_expr r_e@ [BINOP op]

let rec compile p = match p with
    | Language.Stmt.Read variable_name -> [READ; ST variable_name]
    | Language.Stmt.Write expression  -> compile_expr expression @ [WRITE]
    | Language.Stmt.Assign (variable_name, expression) -> compile_expr expression@ [ST variable_name]
    | Language.Stmt.Seq (e1, e2) -> compile e1 @ compile e2;;

