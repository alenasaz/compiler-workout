open GT       
open Language
open List
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env ((stack, ((st, i, o) as c)) as conf) = function
  | [] -> conf
  | inst :: prog_tail ->
       match inst with
       | BINOP op ->
          begin
            match stack with
            | y :: x :: tail ->
               eval env ((Expr.get_oper op x y) :: tail, c) prog_tail
            | _ -> failwith "cannot perform BINOP"
          end
       | CONST v -> eval env (v :: stack, c) prog_tail
       | READ ->
          begin
            match i with
            | x :: tail -> eval env (x :: stack, (st, tail, o)) prog_tail
            | _ -> failwith "cannot perform READ"
          end
       | WRITE ->
          begin
            match stack with
            | x :: tail -> eval env (tail, (st, i, o @ [x])) prog_tail
            | _ -> failwith "cannot perform WRITE"
          end
       | LD x -> eval env ((st x) :: stack, c) prog_tail
       | ST x ->
          begin
            match stack with
            | z :: tail -> eval env (tail, ((Expr.update x z st), i, o)) prog_tail
            | _ -> failwith "cannot perform ST"
          end
       | LABEL l -> eval env conf prog_tail
       | JMP l -> eval env conf (env#labeled l)
       | CJMP (b, l) ->
          begin
            match stack with
            | x :: tail -> if (x = 0 && b = "z" || x != 0 && b = "nz")
                           then eval env (tail, c) (env#labeled l)
                           else eval env (tail, c) prog_tail
            | _ -> failwith "stack is empty"
          end

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o
(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)

let label_generator =
  object
    val mutable counter = 0
    method get =
      counter <- counter + 1;
      "L" ^ string_of_int counter
  end

 let rec compileWithLabels p lastL =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in match p with
  | Stmt.Seq (s1, s2)  -> (let newLabel = label_generator#get in
                           let (compiled1, used1) = compileWithLabels s1 newLabel in
                           let (compiled2, used2) = compileWithLabels s2 lastL in
                           (compiled1 @ (if used1 then [LABEL newLabel] else []) @ compiled2), used2)
  | Stmt.Read x        -> [READ; ST x], false
  | Stmt.Write e       -> (expr e @ [WRITE]), false
  | Stmt.Assign (x, e) -> (expr e @ [ST x]), false
  | Stmt.If (e, s1, s2) ->
    let lElse = label_generator#get in
    let (compiledS1, used1) = compileWithLabels s1 lastL in
    let (compiledS2, used2) = compileWithLabels s2 lastL in
    (expr e @ [CJMP ("z", lElse)]
    @ compiledS1 @ (if used1 then [] else [JMP lastL]) @ [LABEL lElse]
    @ compiledS2 @ (if used2 then [] else [JMP lastL])), true
  | Stmt.While (e, body) ->
    let lCheck = label_generator#get in
    let lLoop = label_generator#get in
    let (doBody, _) = compileWithLabels body lCheck in
    ([JMP lCheck; LABEL lLoop] @ doBody @ [LABEL lCheck] @ expr e @ [CJMP ("nz", lLoop)]), false
  | Stmt.RepeatUntil (body, e) ->
    let lLoop = label_generator#get in
    let (repeatBody, _) = compileWithLabels body lastL in
    ([LABEL lLoop] @ repeatBody @ expr e @ [CJMP ("z", lLoop)]), false
  | Stmt.Skip -> [], false

 let rec compile p =
  let label = label_generator#get in
  let compiled, used = compileWithLabels p label in
  compiled @ (if used then [LABEL label] else []
