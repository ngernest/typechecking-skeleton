(* ** Syntax *)
type term =
    Var of int
  | Lam of term
  | App of term * term
  | Let of term * term

let num_to_hum = function
    | 0 -> "i" | 1 -> "j" | 2 -> "k"
    | 3 -> "l" | 4 -> "m" | 5 -> "a"
    | 6 -> "b" | 7 -> "c" | 8 -> "d"
    | 9 -> "e" | 10 -> "f" | 11 -> "x"
    | 12 -> "y" | 13 -> "z" | 14 -> "w"
    | v -> Format.sprintf "x_%d" v

let fresh_var ctx =
  let var_name = num_to_hum (List.length ctx) in
  var_name, var_name :: ctx

let rec lookup_var ind ctx =
  match ctx with
  | [] -> "UNKNOWN"
  | h :: _ when ind <= 0 -> h
  | _ :: t -> lookup_var (ind - 1) t

let rec pp_term_int ?(needs_parens=false) ctx fmt = function
  | Var n ->
    let var = lookup_var n ctx in
    Format.pp_print_string fmt var
  | Lam body ->
    let (var, ctx) = fresh_var ctx in
    if needs_parens then
      Format.pp_print_string fmt "(";
    Format.pp_print_string fmt "fun ";
    Format.pp_print_string fmt var;
    Format.pp_print_string fmt " -> ";
    pp_term_int ~needs_parens ctx fmt body;
    if needs_parens then
      Format.pp_print_string fmt ")"
  | Let (v1, v2) ->
    if needs_parens then
      Format.pp_print_string fmt "(";
    let (var,ctx') = fresh_var ctx in
    Format.pp_print_string fmt "let ";
    Format.pp_print_string fmt var;
    Format.pp_print_string fmt " = ";
    pp_term_int ~needs_parens:false ctx fmt v1;
    Format.pp_print_string fmt " in ";
    pp_term_int ~needs_parens:false ctx' fmt v2;
    if needs_parens then
      Format.pp_print_string fmt ")"
  | App (v1, v2) ->
    if needs_parens then
      Format.pp_print_string fmt "(";
    pp_term_int ~needs_parens:true ctx fmt v1;
    Format.pp_print_string fmt " ";
    pp_term_int ~needs_parens:true ctx fmt v2;
    if needs_parens then
      Format.pp_print_string fmt ")"
and pp_term fmt = pp_term_int [] fmt
