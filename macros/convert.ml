[@@@warning "-27-26"]
open Ppxlib

let name = "syntax"

let to_str f v = f Format.str_formatter v; Format.flush_str_formatter ()

let build_syntax name (expr : expression) : value_binding list =
  let (module B) = Ast_builder.make expr.pexp_loc in
  let const_of_int ind = B.(pexp_constant (Pconst_integer (string_of_int ind, None))) in
  let extract_arg loc (arg: pattern) =
    let arg_str = Ppxlib_ast.Pprintast.pattern Format.str_formatter arg; Format.flush_str_formatter () in
    match arg.ppat_desc with
    | Ppat_any -> arg_str
    | Ppat_var _ -> arg_str
    | Ppat_construct ({txt=Lident "()"; _}, None ) -> arg_str
    | Ppat_tuple _ -> Location.raise_errorf ~loc "Tuple arguments \"%s\" are not currently supported" arg_str
    | _ -> Location.raise_errorf ~loc "Arguments of the form %s are not supported" arg_str in
  let add_to_env  arg env = arg :: env in
  let lookup_var loc var env =
    let rec loop ind env = match env with
      | [] -> Location.raise_errorf ~loc "Error: Use of undefined variable %s" var
      | h :: t when String.equal var h -> ind
      | _ :: t -> loop (ind + 1) t in
    loop 0 env in

  let enforce_unlabel loc (args: (arg_label * expression) list) =
    let rec loop acc args = match args with
      | [] -> List.rev acc
      | (Nolabel, h) :: t -> loop (h :: acc) t
      | _ -> Location.raise_errorf ~loc "Error: Use of labels in function arguments"
    in
    loop [] args in

  let extract_value_binding loc (vb: value_binding) =
    let arg = extract_arg loc vb.pvb_pat in
    let vl = vb.pvb_expr in
    (arg,vl) in

  let extract_binding_op : location -> binding_op -> 'a * 'b =
    fun loc bop ->
      if not @@ String.equal bop.pbop_op.txt  "let+"
      then
        Location.raise_errorf ~loc "Unsupported binding op %s <> let+" bop.pbop_op.txt
      else
        let arg = extract_arg loc bop.pbop_pat in
        let vl = bop.pbop_exp in
        (arg,vl) in

  let build_variable loc ind =
    B.(pexp_construct {txt=Lident "Var";loc} (Some (const_of_int ind))) in

  let build_const loc txt =
    B.(pexp_construct {txt=Lident "Const";loc}
         (Some (pexp_constant (Pconst_integer (txt, None))))) in

  let rec exprs_to_list args =
    let loc = expr.pexp_loc in
    match args with
    | [] -> [%expr []]
    | h :: t -> [%expr [%e h] :: [%e exprs_to_list t]] in

  let build_apply loc func args =
    B.(pexp_construct {txt=Lident "App";loc} (Some (pexp_tuple ( func ::  args ))))
  in

  let build_lam loc expr =
    B.(pexp_construct {txt=Lident "Lam";loc} (Some (expr))) in

  let build_lbind : location -> expression -> expression -> expression =
    fun loc vl body ->
      (* [%expr App (Lam [%e body], [%e vl])] *)
      [%expr Let ([%e vl], [%e body])] in

  let build_ifthenelse : location -> expression -> expression -> expression -> expression =
    fun loc cond ifT ifF -> 
      B.(pexp_construct {txt=Lident "IfThenElse";loc} (Some (
          pexp_tuple [
            cond;
            ifT;
            ifF
          ]
        ))) in


  let lident_to_string lident =
    to_str Ppxlib_ast.Pprintast.expression
      (B.pexp_ident  { txt=lident; loc=expr.pexp_loc }) in

  let rec convert_expr env expr : expression = 
    let error = Location.raise_errorf ~loc:expr.pexp_loc in
    (match expr with
     | { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
       (match pexp_desc with

        | Pexp_ident { txt=Lident txt; loc } ->
          let ind = lookup_var pexp_loc txt env in
          build_variable pexp_loc ind

        | Pexp_ident { txt=_; loc } -> Location.raise_errorf ~loc "Complex identifiers %s aren't supported" (to_str Ppxlib_ast.Pprintast.expression expr)

        | Pexp_constant (Pconst_integer (txt, _)) ->
          build_const pexp_loc txt

        | Pexp_constant _ ->
          error "Non integer constants aren't supported at the moment"

        | Pexp_let (Recursive, _, _) ->
          error "lol haven't implemented this [recursive let]"

        | Pexp_let (Nonrecursive, vb :: [], body) ->
          let (arg, expr) = extract_value_binding pexp_loc vb in
          let expr = convert_expr env expr in
          let env = add_to_env arg env in
          let body = convert_expr env body in
          build_lbind pexp_loc expr body

        | Pexp_let (Nonrecursive, _, body) ->
          error "Parallel value bindings aren't supported"

        | Pexp_fun (Nolabel, None, arg, body) ->
          let arg = extract_arg pexp_loc arg in
          let env = add_to_env arg env in
          let body = convert_expr env body in
          build_lam pexp_loc body

        | Pexp_fun (_, _, _, _) -> error "labels/optional arguments to \
                                          functions aren't supported"

        | Pexp_apply (func, args) ->
          let expr = convert_expr env func in
          let args = enforce_unlabel pexp_loc args in
          let args = List.map (convert_expr env) args in
          build_apply pexp_loc expr args

        (* | Pexp_apply (_, _) -> error "Only application to constant \
         *                               functions are supported." *)

        | Pexp_letop { let_=_; ands=_; body=_ } ->
          error "Parallel value bindings aren't supported"

        (* | Pexp_function _ -> (??) *)
        (* | Pexp_match (_, _) -> error "lol haven't implemented this [ident]" *)
        | Pexp_tuple _ -> error "lol haven't implemented this [tuple]"

        | Pexp_construct (_, _) -> error "lol haven't implemented this [construct]"

        | Pexp_record (_, _) -> error "lol haven't implemented this [record]"

        | Pexp_field (_, _) -> error "lol haven't implemented this [field]"

        | Pexp_ifthenelse (cond, e1, Some e2) ->
          let cond = convert_expr env cond
          and e1 = convert_expr env e1
          and e2 = convert_expr env e2 in
          build_ifthenelse pexp_loc cond e1 e2

        | Pexp_ifthenelse (_, _, _) ->
          error "If clauses must have an else!"

        | Pexp_poly (_, _) -> error "lol haven't implemented this [poly??]"

        | _ -> 
          Location.raise_errorf ~loc:expr.pexp_loc "Use of unsupported syntactic construct %s"
            (Ppxlib_ast.Pprintast.expression Format.str_formatter expr; Format.flush_str_formatter ())
       )) in

  let rec convert_def env expr = 
    (match expr with
     | { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
       (match pexp_desc with
        | Pexp_fun (Nolabel, None, arg, body) ->
          let arg = extract_arg pexp_loc arg in
          let env = add_to_env arg env in
          convert_def env body
        | Pexp_fun (_, _, _, _) -> Location.raise_errorf ~loc:pexp_loc "labels/optional arguments on functions aren't supported"
        | _ ->
          env, convert_expr env expr
       )) in
  let (args, body) = convert_def [] expr in
  let func = let rec iter n f v = if n <= 0 then v else iter (n - 1) f (f v) in
    let loc = expr.pexp_loc in
    iter (List.length args) (fun v -> [%expr Lam [%e v]]) body in
  [ B.(value_binding ~pat:(ppat_var name) ~expr:func) ]

let build_syntax_named name (expr : expression) : value_binding list =
  let (module B) = Ast_builder.make expr.pexp_loc in
  let const_of_int ind = B.(pexp_constant (Pconst_integer (string_of_int ind, None))) in
  let const_of_string str = B.pexp_constant (Pconst_string (str, expr.pexp_loc, None)) in
  let extract_arg loc (arg: pattern) =
    let arg_str = Ppxlib_ast.Pprintast.pattern Format.str_formatter arg; Format.flush_str_formatter () in
    match arg.ppat_desc with
    | Ppat_any -> arg_str
    | Ppat_var _ -> arg_str
    | Ppat_construct ({txt=Lident "()"; _}, None ) -> arg_str
    | Ppat_tuple _ -> Location.raise_errorf ~loc "Tuple arguments \"%s\" are not currently supported" arg_str
    | _ -> Location.raise_errorf ~loc "Arguments of the form %s are not supported" arg_str in

  let enforce_unlabel loc (args: (arg_label * expression) list) =
    let rec loop acc args = match args with
      | [] -> List.rev acc
      | (Nolabel, h) :: t -> loop (h :: acc) t
      | _ -> Location.raise_errorf ~loc "Error: Use of labels in function arguments"
    in
    loop [] args in

  let extract_value_binding loc (vb: value_binding) =
    let arg = extract_arg loc vb.pvb_pat in
    let vl = vb.pvb_expr in
    (arg,vl) in

  let extract_binding_op : location -> binding_op -> 'a * 'b =
    fun loc bop ->
      if not @@ String.equal bop.pbop_op.txt  "let+"
      then
        Location.raise_errorf ~loc "Unsupported binding op %s <> let+" bop.pbop_op.txt
      else
        let arg = extract_arg loc bop.pbop_pat in
        let vl = bop.pbop_exp in
        (arg,vl) in

  let build_variable loc ind =
    B.(pexp_construct {txt=Lident "Var";loc} (Some (const_of_string ind))) in

  let build_const loc txt =
    B.(pexp_construct {txt=Lident "Const";loc}
         (Some (pexp_constant (Pconst_integer (txt, None))))) in

  let rec exprs_to_list args =
    let loc = expr.pexp_loc in
    match args with
    | [] -> [%expr []]
    | h :: t -> [%expr [%e h] :: [%e exprs_to_list t]] in

  let build_apply loc func args =
    B.(pexp_construct {txt=Lident "App";loc} (Some (pexp_tuple ( func ::  args ))))
  in

  let build_lam loc var expr = [%expr Lam ([%e const_of_string var], [%e expr])]  in

  let build_lbind : location -> string -> expression -> expression -> expression =
    fun loc arg vl body ->
      (* [%expr App (Lam [%e body], [%e vl])] *)
      [%expr Let ([%e const_of_string arg], [%e vl], [%e body])] in

  let build_ifthenelse : location -> expression -> expression -> expression -> expression =
    fun loc cond ifT ifF -> 
      B.(pexp_construct {txt=Lident "IfThenElse";loc} (Some (
          pexp_tuple [
            cond;
            ifT;
            ifF
          ]
        ))) in


  let lident_to_string lident =
    to_str Ppxlib_ast.Pprintast.expression
      (B.pexp_ident  { txt=lident; loc=expr.pexp_loc }) in

  let rec convert_expr env expr : expression = 
    let error = Location.raise_errorf ~loc:expr.pexp_loc in
    (match expr with
     | { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
       (match pexp_desc with

        | Pexp_ident { txt=Lident txt; loc } ->
          build_variable pexp_loc txt

        | Pexp_ident { txt=_; loc } -> Location.raise_errorf ~loc "Complex identifiers %s aren't supported" (to_str Ppxlib_ast.Pprintast.expression expr)

        | Pexp_constant (Pconst_integer (txt, _)) ->
          build_const pexp_loc txt

        | Pexp_constant _ ->
          error "Non integer constants aren't supported at the moment"

        | Pexp_let (Recursive, _, _) ->
          error "lol haven't implemented this [recursive let]"

        | Pexp_let (Nonrecursive, vb :: [], body) ->
          let (arg, expr) = extract_value_binding pexp_loc vb in
          let expr = convert_expr env expr in
          let body = convert_expr env body in
          build_lbind pexp_loc arg expr body

        | Pexp_let (Nonrecursive, _, body) ->
          error "Parallel value bindings aren't supported"

        | Pexp_fun (Nolabel, None, arg, body) ->
          let arg = extract_arg pexp_loc arg in
          let body = convert_expr env body in
          build_lam pexp_loc arg body

        | Pexp_fun (_, _, _, _) -> error "labels/optional arguments to \
                                          functions aren't supported"

        | Pexp_apply (func, args) ->
          let expr = convert_expr env func in
          let args = enforce_unlabel pexp_loc args in
          let args = List.map (convert_expr env) args in
          build_apply pexp_loc expr args

        (* | Pexp_apply (_, _) -> error "Only application to constant \
         *                               functions are supported." *)

        | Pexp_letop { let_=_; ands=_; body=_ } ->
          error "Parallel value bindings aren't supported"

        (* | Pexp_function _ -> (??) *)
        (* | Pexp_match (_, _) -> error "lol haven't implemented this [ident]" *)
        | Pexp_tuple _ -> error "lol haven't implemented this [tuple]"

        | Pexp_construct (_, _) -> error "lol haven't implemented this [construct]"

        | Pexp_record (_, _) -> error "lol haven't implemented this [record]"

        | Pexp_field (_, _) -> error "lol haven't implemented this [field]"

        | Pexp_ifthenelse (cond, e1, Some e2) ->
          let cond = convert_expr env cond
          and e1 = convert_expr env e1
          and e2 = convert_expr env e2 in
          build_ifthenelse pexp_loc cond e1 e2

        | Pexp_ifthenelse (_, _, _) ->
          error "If clauses must have an else!"

        | Pexp_poly (_, _) -> error "lol haven't implemented this [poly??]"

        | _ -> 
          Location.raise_errorf ~loc:expr.pexp_loc "Use of unsupported syntactic construct %s"
            (Ppxlib_ast.Pprintast.expression Format.str_formatter expr; Format.flush_str_formatter ())
       )) in

  let rec convert_def env expr = 
    (match expr with
     | { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
       (match pexp_desc with
        | Pexp_fun (Nolabel, None, arg, body) ->
          let arg = extract_arg pexp_loc arg in
          convert_def (arg :: env) body
        | Pexp_fun (_, _, _, _) -> Location.raise_errorf ~loc:pexp_loc "labels/optional arguments on functions aren't supported"
        | _ ->
          env, convert_expr env expr
       )) in

  let (args, body) = convert_def [] expr in
  let func = let rec iter n f v = if n <= 0 then v else iter (n - 1) f (f v) in
    let loc = expr.pexp_loc in
    List.fold_right (fun arg v -> [%expr Lam ([%e const_of_string arg ], v)]) args body
     in
  [ B.(value_binding ~pat:(ppat_var name) ~expr:func) ]


let convert_extension =
  Extension.V3.declare name
    Extension.Context.structure_item
    Ast_pattern.(
      pstr ((pstr_value nonrecursive ((value_binding ~pat:(ppat_var __') ~expr:__) ^:: nil)) ^:: nil)
    )
    (fun ~ctxt:_ name value ->
       let bindings = build_syntax name value in
       let (module B) = Ast_builder.make name.loc in
       let open B in
       pstr_value Nonrecursive bindings
    )



let rules : Context_free.Rule.t list = [
  Context_free.Rule.extension convert_extension
]

let () =
  Driver.register_transformation ~rules name