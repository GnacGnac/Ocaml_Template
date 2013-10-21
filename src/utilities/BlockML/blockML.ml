
open Result


module Generic = Block_generic


module ChildrenSpec = Children_spec

type 'node occurrence_error = 'node Block_instance.occurrence_error
type 'node analyze_error = 'node Block_instance.analyze_error
type 'node parse_error = 'node Block_instance.parse_error

type 'node grammar_parse_error =
['node parse_error | `Grammar_unrecognized_node of string Position.t]

module Instance = Block_instance


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
    | Primitive
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
       (Un_con, "un_con") ;
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
	  [S.one_int ;
	   S.one Primitive ;
	   S.one_text ;
	   S.and_ [S.one Bin_op ; S.exact 2 Exp]]
      | Primitive -> S.sum_one [Int ; Text]
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

    let apply_if_mem_node block (node, f) = G.get_node node block >>= f block

    let bin_cmp_of_block block =
      assert false (* TODO *)

    let un_con_of_block block =
      assert false (* TODO *)

    let bin_con_of_block block =
      assert false (* TODO *)

    let bin_cmp_spec_of_block parent_block bin_cmp_block =
      assert false (* TODO *)

    let rec spec_of_block block =
      let f = function
	| Ok spec -> Some spec
	| Error `No_such_child -> None in
      let nodes =
	[(Bin_cmp, bin_con_spec_of_block) ;
	 (Un_con, un_con_spec_of_block)] in
      let nodes = List.map (apply_if_mem_node block) nodes in
      extract (List_ext.find_and_apply f nodes)

    and un_con_spec_of_block parent_block un_con_block =
      assert false (* TODO *)

    and bin_con_spec_of_block parent_block bin_con_block =
      assert false (* TODO *)

    let add_children_spec children_specs block =
      let name = G.extract_text_with_pos (G.extract_node Node block) in
      of_string name >>= fun name ->
      let spec = spec_of_block (G.extract_node Spec block) in
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
