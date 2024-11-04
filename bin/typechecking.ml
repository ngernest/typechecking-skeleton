open Lib.Syntax


type typ =
    | Var of var ref
    | QVar of int
    | Arrow of typ * typ
and var = Unbound of int | Link of typ


let rec typeof env : term -> typ = failwith "write your typechecking/inference code here"


(* example: *)
(* ** Example *)
let%syntax f x = 
  let y = fun f -> f x in
  y (fun x -> x)
