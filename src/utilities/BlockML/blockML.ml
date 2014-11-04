
open Result


module Generic = Block_generic


module ChildrenSpec = Children_spec

type 'node occurrence_error = 'node Block_instance.occurrence_error
type 'node analyze_error = 'node Block_instance.analyze_error
type 'node parse_error = 'node Block_instance.parse_error

type save_error = [`Could_not_write_file of string]


module Instance = Block_instance


type 'node grammar_parse_error =
['node parse_error | `Grammar_unrecognized_node of string Position.t]

module Grammar = struct

  type node =
    | Grammar
    | Possible_roots
    | Children_specs
    | Children_spec
    | Node
    | Spec
    | Bin_cmp
    | Un_con
    | Bin_con
    | True
    | False
    | And
    | Or
    | Not
    | Eq
    | Diff
    | Le
    | Lt
    | Ge
    | Gt
    | Exp
    | Cst
    | Primitive
    | Var
    | Bin_op
    | Add
    | Sub
    | Mul
    | Int
    | Text

  module M = struct
    type t = node
    let string_assoc =
      [(Grammar, "grammar") ; (Possible_roots, "possible_roots") ;
       (Children_specs, "children_specs") ; (Children_spec, "children_spec") ;
       (Node, "node") ; (Spec, "spec") ; (Bin_cmp, "bin_cmp") ;
       (Un_con, "un_con") ; (Cst, "cst") ; (Var, "var") ;
       (Bin_con, "bin_con") ; (True, "true") ; (False, "false") ; (And, "and") ;
       (Or, "or") ; (Not, "not") ; (Eq, "eq") ; (Diff, "diff") ; (Le, "le") ;
       (Lt, "lt") ; (Ge, "ge") ; (Gt, "gt") ; (Exp, "exp") ;
       (Primitive, "primitive") ; (Bin_op, "bin_op") ;
       (Add, "add") ; (Sub, "sub") ; (Mul, "mul") ; (Int, "int") ;
       (Text, "text")]
    let all = List.map fst string_assoc
  end

  module type S = sig
    type t
    module type S = Instance.S with type Node.t = t
    val from_file :
      string -> ((module S), [> node grammar_parse_error]) Result.t
  end

  module Spec = struct

    include M

    let possible_roots = [Grammar]

    module S = ChildrenSpec.Make (M)

    let spec = function
      | Grammar -> S.only (S.ones [Possible_roots ; Children_specs])
      | Possible_roots -> S.only S.any_text
      | Children_specs -> S.only (S.any Children_spec)
      | Children_spec -> S.only (S.ones [Node ; Spec])
      | Node -> S.only S.one_text
      | Spec ->
	S.sum
	  [S.and_ [S.one Bin_cmp ; S.exact 2 Exp] ;
	   S.ones [Un_con ; Spec] ;
	   S.and_ [S.one Bin_con ; S.any Spec] ;
	   S.one True ;
	   S.one False]
      | Bin_cmp -> S.sum_one [Eq ; Diff ; Le ; Lt ; Ge ; Gt]
      | Un_con -> S.sum_one [Not]
      | Bin_con -> S.sum_one [And ; Or]
      | True | False | And | Or | Not | Eq | Diff | Le | Lt | Ge | Gt | Add
      | Sub | Mul | Int | Text -> S.nones
      | Exp ->
	S.sum
	  [S.one Cst ;
	   S.one Primitive ;
	   S.one Var ;
	   S.and_ [S.one Bin_op ; S.exact 2 Exp]]
      | Cst -> S.only S.one_int
      | Primitive -> S.sum_one [Int ; Text]
      | Var -> S.only S.one_text
      | Bin_op -> S.sum_one [Add ; Sub ; Mul]

  end

  module G = Instance.MakeUnsafe (Spec)

  module type M = sig
    include String_ext.STRINGABLE
    val compare : t -> t -> int
  end

  module Make (M : M) = struct

    module type S = Instance.S with type Node.t = M.t

    type t = M.t

    module MSet = Set_ext.Make (M)
    module MMap = Map_ext.Make (M)

    let of_string pos_string =
      map_error
	(function
	| `Unrecognized_string _ -> `Grammar_unrecognized_node pos_string)
	(M.of_string (Position.contents pos_string))

    let possible_roots block =
      let possible_roots = G.extract_node Possible_roots block in
      let possible_roots = G.extract_text_children_with_pos possible_roots in
      List_ext.bind of_string possible_roots

    let apply_if_mem_node block (node, f) =
      Option.map (f block) (to_option (G.get_node node block))

    let enum_element_of_block nodes block =
      let f (node, enum_element) =
	apply_if_mem_node block (node, (fun _ _ -> return enum_element)) in
      extract (extract (List_ext.find_and_apply f nodes))

    let cst_spec_of_block parent_block cst_block =
      return (ChildrenSpec.Cst (G.extract_int cst_block))

    let primitive_of_block =
      enum_element_of_block
	[(Int, ChildrenSpec.Int) ; (Text, ChildrenSpec.Text)]

    let primitive_spec_of_block parent_block primitive_block =
      return (ChildrenSpec.Primitive (primitive_of_block primitive_block))

    let var_spec_of_block parent_block var_block =
      of_string (G.extract_text_with_pos var_block) >>= fun node ->
      return (ChildrenSpec.Var node)

    let bin_op_of_block =
      enum_element_of_block
	[(Add, ChildrenSpec.Add) ; (Sub, ChildrenSpec.Sub) ;
	 (Mul, ChildrenSpec.Mul)]

    let rec exp_of_block block =
      let nodes =
	[(Cst, cst_spec_of_block) ;
	 (Primitive, primitive_spec_of_block) ;
	 (Var, var_spec_of_block) ;
	 (Bin_op, bin_op_spec_of_block)] in
      extract (List_ext.find_and_apply (apply_if_mem_node block) nodes)

    and bin_op_spec_of_block parent_block bin_op_block =
      let bin_op = bin_op_of_block bin_op_block in
      let exps = G.extract_node_children Exp parent_block in
      List_ext.bind exp_of_block exps >>= function
      | [e1 ; e2] -> return (ChildrenSpec.Bin_op (bin_op, e1, e2))
      | _ ->
	(* Should not be possible because of the node specification. *)
	assert false

    let bin_cmp_of_block =
      enum_element_of_block
	[(Eq, ChildrenSpec.Eq) ; (Diff, ChildrenSpec.Diff) ;
	 (Le, ChildrenSpec.Le) ; (Lt, ChildrenSpec.Lt) ;
	 (Ge, ChildrenSpec.Ge) ; (Gt, ChildrenSpec.Gt)]

    let bin_cmp_spec_of_block parent_block bin_cmp_block =
      let bin_cmp = bin_cmp_of_block bin_cmp_block in
      let exps = G.extract_node_children Exp parent_block in
      List_ext.bind exp_of_block exps >>= function
      | [e1 ; e2] -> return (ChildrenSpec.Bin_cmp (bin_cmp, e1, e2))
      | _ ->
	(* Should not be possible because of the node specification. *)
	assert false

    let un_con_of_block = enum_element_of_block [(Not, ChildrenSpec.Not)]

    let bin_con_of_block =
      enum_element_of_block [(And, ChildrenSpec.And) ; (Or, ChildrenSpec.Or)]

    let rec spec_of_block block =
      let nodes =
	[(Bin_cmp, bin_cmp_spec_of_block) ;
	 (Un_con, un_con_spec_of_block) ;
	 (Bin_con, bin_con_spec_of_block) ;
	 (True, true_spec_of_block) ;
	 (False, false_spec_of_block)] in
      extract (List_ext.find_and_apply (apply_if_mem_node block) nodes)

    and un_con_spec_of_block parent_block un_con_block =
      let un_con = un_con_of_block un_con_block in
      spec_of_block (G.extract_node Spec parent_block) >>= fun spec ->
      return (ChildrenSpec.Un_con (un_con, spec))

    and bin_con_spec_of_block parent_block bin_con_block =
      let bin_con = bin_con_of_block bin_con_block in
      let specs = G.extract_node_children Spec parent_block in
      List_ext.bind spec_of_block specs >>= fun specs ->
      return (ChildrenSpec.Bin_con (bin_con, specs))

    and true_spec_of_block parent_block bin_con_block =
      return ChildrenSpec.True

    and false_spec_of_block parent_block bin_con_block =
      return ChildrenSpec.False

    let add_children_spec children_specs block =
      let name = G.extract_text_with_pos (G.extract_node Node block) in
      of_string name >>= fun name ->
      spec_of_block (G.extract_node Spec block) >>= fun spec ->
      return (MMap.add name spec children_specs)

    let children_specs block =
      let children_specs = G.extract_node Children_specs block in
      let children_specs =
	G.extract_node_children Children_spec children_specs in
      List_ext.fold_bind add_children_spec MMap.empty children_specs

    let from_file file =
      G.parse file >>= fun block ->
      possible_roots block >>= fun possible_roots ->
      children_specs block >>= fun children_specs ->
      let module Spec = struct

	include M

	module Set = MSet
	module Map = MMap

	let possible_roots = Set.of_list possible_roots

	let spec node = match Map.find node children_specs with
	  | Ok children_spec -> children_spec
	  | Error `Not_found -> ChildrenSpec.True

      end in
      return (module Instance.Make (Spec) : S)

  end

  module MakeUnsafe (M : String_ext.UNSAFE_STRINGABLE) = struct

    module M' = struct
      include String_ext.MakeStringable (M)
      let compare = Pervasives.compare
    end

    include Make (M')

  end

  include G

end


module type PARSE_RESULT = sig
  include Instance.S
  val parse_result : (t, [> Node.t parse_error]) Result.t
end

let parse_from_external unsafe_stringable grammar_file file =
  let module M = (val unsafe_stringable : String_ext.UNSAFE_STRINGABLE) in
  let module Grammar = Grammar.MakeUnsafe (M) in
  match Grammar.from_file grammar_file with
  | Ok instance ->
    let module Instance = struct
      include (val instance : Grammar.S)
      let parse_result = parse file
    end in
    return (module Instance : PARSE_RESULT)
  | Error err -> error (`Grammar_error err)
