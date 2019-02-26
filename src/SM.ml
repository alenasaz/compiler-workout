open GT       
       
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
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let eval_i config insn= 
        let (stack, s_config) = config in 
        let (state, i, o) = s_config in
        match insn with
         |BINOP operator -> (match stack with
                |y::x::tail -> ([(Syntax.Expr.get_operator operator) x y]@tail, s_config)
         |CONST value -> ([value]@stack, s_config)
         |READ - > (match i with 
                  |head::tail->([head]@stack, (state, tail, o)))
         |WRITE -> (match stack with
                  |head::tail -> (tail (state, i, o@[head])))
         |LD var ->([state var]@stack, s_config)
         |ST var ->(match stack with 
                |head::tail -> (tail,(Syntax.Expr.update var head stack, i, o)))


let eval config prg = List.fold_left eval_i config prg
(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile_expr e = match e with
    | Syntax.Expr.Const x -> [CONST x]
    | Syntax.Expr.Var n -> [LD n]
    | Syntax.Expr.Binop (op, e1, e2) -> compile_expr e1 @ compile_expr e2 @ [BINOP op]

let rec compile p = match p with
    | Syntax.Stmt.Read var -> [READ; ST var]
    | Syntax.Stmt.Write expr -> compile_expr expr @ [WRITE]
    | Syntax.Stmt.Assign (var, expr) -> compile_expr expr @ [ST var]
    | Syntax.Stmt.Seq (e1, e2) -> compile e1 @ compile e2