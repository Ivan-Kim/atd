(*
   Kotlin code generation for JSON support (no biniou support)

   Takes the contents of a .atd file and translates it to a .kt file.
   Look into the tests to see what generated code looks like.
*)

open Printf
open Atd.Ast
open Indent
module A = Atd.Ast
module B = Indent

(* Mutable environment holding hash tables and such to avoid
   naming conflicts. *)
type env = {
  (* Global *)
  create_variable: string -> string;
  translate_variable: string -> string;
  (* Local to a class: instance variables, including method names *)
  translate_inst_variable: unit -> (string -> string);
}

let annot_schema_kotlin : Atd.Annot.schema_section =
  {
    section = "kotlin";
    fields = [
      Module_head, "text";
      Module_head, "json_kt.text";
      Type_expr, "repr";
      Field, "default";
    ]
  }

let annot_schema : Atd.Annot.schema =
  annot_schema_kotlin :: Atd.Json.annot_schema_json

(* Translate a preferred variable name into an available Kotlin identifier. *)
let trans env id =
  env.translate_variable id

(*
   Convert an ascii string to CamelCase.
   Note that this gets rid of leading and trailing underscores.
*)
let to_camel_case capitalize s =
  let buf = Buffer.create (String.length s) in
  let start_word = ref true in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '_' ->
        start_word := true
    | 'a'..'z' as c when !start_word ->
        let cap = if (capitalize || i <> 0) then Char.uppercase_ascii else Char.lowercase_ascii in
        Buffer.add_char buf (cap c);
        start_word := false
    | '\'' ->
          Buffer.add_char buf '_';
          start_word := false
    | c ->
        Buffer.add_char buf c;
        start_word := false
  done;
  let name = Buffer.contents buf in
  if name = "" then "X"
  else
    (* Make sure we don't start with a digit. This happens with
       generated identifiers like '_42'. *)
    match name.[0] with
    | 'A'..'Z' | 'a'..'z' | '_' -> name
    | _ -> "X" ^ name

(* Use CamelCase as recommended by Kotlin style guide. *)
let class_name env id =
  trans env (to_camel_case true id)

(*
   Create a class identifier that hasn't been seen yet.
   This is for internal disambiguation and still must translated using
   the 'trans' function ('class_name' will not work due to trailing
   underscores being added for disambiguation).
*)
let create_class_name env name =
  let preferred_id = to_camel_case true name in
  env.create_variable preferred_id

let init_env () : env =
  let keywords = [
    (* Keywords
       https://kotlinlang.org/docs/keyword-reference.html
    *)
    "as"; "as?"; "break"; "class"; "continue";
    "do"; "else"; "false"; "for"; "fun";
    "if"; "in"; "!in"; "interface"; "is"; "!is";
    "null"; "object"; "package"; "return"; "super"; "this";
    "throw"; "true"; "try"; "typealias"; "typeof";
    "val"; "var"; "when"; "while";

    (* Soft keywords
       https://kotlinlang.org/docs/keyword-reference.html#soft-keywords
    *)
    "by"; "catch"; "constructor"; "delegate"; "dynamic";
    "field"; "file"; "finally"; "get"; "import";
    "init"; "param"; "property"; "receiver"; "set";
    "setparam"; "value"; "where";
  ]
  in
  (* Various variables used in the generated code.
     Lowercase variables in this list are superfluous as long as all generated
     variables either start with '_', 'atd_', or an uppercase letter.
  *)
  let reserved_variables = [
    (* from typing *)
    "Map"; "List"; "Pair"; "Triple";

    (* for use in kotlinx.serialization types *)
    "Json"; "JsonElement";

    (* exceptions *)
    "SerializationException";

    (* used to check JSON node type *)
    "Boolean"; "Int"; "Double"; "String";

    (* other built-in variables *)
    "this";
  ] in
  let variables =
    Atd.Unique_name.init
      ~reserved_identifiers:(reserved_variables @ keywords)
      ~reserved_prefixes:["atd_"; "_atd_"]
      ~safe_prefix:"x_"
  in
  let method_names () =
    Atd.Unique_name.init
      ~reserved_identifiers:(
        ["fromJson"; "toJson";
         "fromJsonString"; "toJsonString"]
        @ keywords
      )
      ~reserved_prefixes:["__"]
      ~safe_prefix:"x_"
  in
  let create_variable name =
    Atd.Unique_name.create variables name
  in
  let translate_variable id =
    Atd.Unique_name.translate variables id
  in
  let translate_inst_variable () =
    let u = method_names () in
    fun id -> Atd.Unique_name.translate u id
  in
  {
    create_variable;
    translate_variable;
    translate_inst_variable;
  }

type quote_kind = Single | Double

(* Escape a string fragment to be placed in single quotes or double quotes.
*)
let escape_string_content quote_kind s =
  let buf = Buffer.create (String.length s + 2) in
  for i = 0 to String.length s - 1 do
    match s.[i], quote_kind with
    | '\n', _ -> Buffer.add_string buf "\\n"
    | '\\', _ -> Buffer.add_string buf "\\\\"
    | '\'', Single -> Buffer.add_string buf "\\'"
    | '"', Double -> Buffer.add_string buf "\\\""
    | c, (Single | Double) -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let single_esc s =
  escape_string_content Single s

let _double_esc s =
  escape_string_content Double s

let fixed_size_preamble atd_filename =
  sprintf {|// placeholder package declaration, change accordingly to use case
package org.example
  
/* Generated by atdk from type definitions in %s.

This implements classes for the types defined in '%s', providing
methods and functions to convert data from/to JSON.
*/

import kotlinx.serialization.*
import kotlinx.serialization.json.*

// ############################################################################
// # Private functions
// ############################################################################


// ############################################################################
// # Public classes
// ############################################################################|}
    atd_filename
    atd_filename

let not_implemented loc msg =
  A.error_at loc ("not implemented in atdk: " ^ msg)

let todo hint =
  failwith ("TODO: " ^ hint)

let spaced ?(spacer = [Line ""]) (blocks : B.node list) : B.node list =
  let rec spaced xs =
    match List.filter (fun x -> not (B.is_empty_node x)) xs with
    | []
    | [_] as xs -> xs
    | a :: rest -> a :: spacer @ spaced rest
  in
  spaced blocks

let double_spaced blocks =
  spaced ~spacer:[Line ""; Line ""] blocks

(*
   Representations of ATD type '(string * value) list' in JSON and Kotlin.
   Key type or value type are provided when it's useful.
*)
type assoc_kind =
  | Array_list (* default representation; possibly not even a list of pairs *)
  | Array_dict of type_expr * type_expr (* key type, value type *)
  (* Keys in JSON objects are always of type string. *)
  | Object_dict of type_expr (* value type *)
  | Object_list of type_expr (* value type *)

let assoc_kind loc (e : type_expr) an : assoc_kind =
  let json_repr = Atd.Json.get_json_list an in
  let kotlin_repr = Kotlin_annot.get_kotlin_assoc_repr an in
  match e, json_repr, kotlin_repr with
  | Tuple (loc, [(_, key, _); (_, value, _)], an2), Array, Map ->
      Array_dict (key, value)
  | Tuple (loc,
           [(_, Name (_, (_, "string", _), _), _); (_, value, _)], an2),
    Object, Map ->
      Object_dict value
  | Tuple (loc,
           [(_, Name (_, (_, "string", _), _), _); (_, value, _)], an2),
    Object, List -> Object_list value
  | _, Array, List -> Array_list
  | _, Object, _ -> error_at loc "not a (string * _) list"
  | _, Array, _ -> error_at loc "not a (_ * _) list"

(* Map ATD built-in types to built-in Kotlin types *)
let kt_type_name env (name : string) =
  match name with
  | "unit" -> "Unit"
  | "bool" -> "Boolean"
  | "int" -> "Int"
  | "float" -> "Double"
  | "string" -> "String"
  | "abstract" -> "JsonElement"
  | user_defined -> class_name env user_defined

let rec type_name_of_expr env (e : type_expr) : string =
  match e with
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, xs, an) ->
      let type_names =
        xs
        |> List.map (fun (loc, x, an) -> type_name_of_expr env x)
      in
      (* Kotlin does not support tuples as builtin language construct *)
      let tuple_type = match List.length type_names with
      | 2 -> "Pair"
      | 3 -> "Triple" 
      | _ -> not_implemented loc "tuples of more than three elements"
      in
      sprintf "%s<%s>" tuple_type (String.concat ", " type_names)
  | List (loc, e, an) ->
     (match assoc_kind loc e an with
       | Array_list
       | Object_list _ ->
           sprintf "List<%s>"
             (type_name_of_expr env e)
       | Array_dict (key, value) ->
           sprintf "Map<%s, %s>"
             (type_name_of_expr env key) (type_name_of_expr env value)
       | Object_dict value ->
           sprintf "Map<String, %s>"
             (type_name_of_expr env value)
      )
  | Option (loc, e, an) -> sprintf "%s?" (type_name_of_expr env e)
  | Nullable (loc, e, an) -> sprintf "%s?" (type_name_of_expr env e)
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> todo "wrap"
  | Name (loc, (loc2, name, []), an) -> kt_type_name env name
  | Name (loc, (_, name, _::_), _) -> assert false
  | Tvar (loc, _) -> not_implemented loc "type variables"

let rec get_default_default (e : type_expr) : string option =
  match e with
  | Sum _
  | Record _
  | Tuple _ (* a default tuple could be possible but we're lazy *) -> None
  | List _ -> Some "listOf()"
  | Option _
  | Nullable _ -> Some "null"
  | Shared (loc, e, an) -> get_default_default e
  | Wrap (loc, e, an) -> get_default_default e
  | Name (loc, (loc2, name, []), an) ->
      (match name with
       | "unit" -> None
       | "bool" -> Some "false"
       | "int" -> Some "0"
       | "float" -> Some "0.0"
       | "string" -> Some {|""|}
       | "abstract" -> None
       | _ -> None
      )
  | Name _ -> None
  | Tvar _ -> None

let get_kotlin_default (e : type_expr) (an : annot) : string option =
  let user_default = Kotlin_annot.get_kotlin_default an in
  match user_default with
  | Some s -> Some s
  | None -> get_default_default e

(* see explanation where this function is used *)
let has_no_class_inst_prop_default
    ((loc, (name, kind, an), e) : simple_field) =
  match kind with
  | Required -> true
  | Optional -> (* default is None *) false
  | With_default ->
      match get_kotlin_default e an with
      | Some _ -> false
      | None ->
          (* There's either no default at all which is an error,
             or the default value is known to be mutable. *)
          true

(* If the field is '?foo: bar option', its kotlin or json value has type
   'bar' rather than 'bar option'. *)
let unwrap_field_type loc field_name kind e =
  match kind with
  | Required
  | With_default -> e
  | Optional ->
      match e with
      | Option (loc, e, an)
      | Nullable (loc, e, an) -> e
      | _ ->
          A.error_at loc
            (sprintf "the type of optional field '%s' should be of \
                      the form 'xxx option'" field_name)

(*
   Instance variable that's really the name of the getter method created
   by @dataclass. It can't start with '__' as those are reserved for
   internal magic. The 'trans_meth' translator must take care of this.
*)
let inst_var_name trans_meth field_name =
  trans_meth field_name

let inst_var_declaration
    env trans_meth ((loc, (name, kind, an), e) : simple_field) =
  let var_name = inst_var_name trans_meth name |> to_camel_case false in
  let type_name = type_name_of_expr env e in
  let unwrapped_e = unwrap_field_type loc name kind e in
  let default =
    match kind with
    | Required -> ""
    | Optional -> " = null"
    | With_default ->
        match get_kotlin_default unwrapped_e an with
        | None -> ""
        | Some x -> sprintf " = %s" x
  in
  [
    Line (sprintf "val %s: %s%s," var_name type_name default)
  ]

let to_json_methods name =
  let to_json =
    [
      Line "fun toJson(): JsonElement {";
      Block [
        Line "return Json.encodeToJsonElement(serializer(), this)"
      ];
      Line "}";
    ]
  in
  let to_json_string =
    [
      Line "fun toJsonString(): String {";
      Block [
        Line "return Json.encodeToString(serializer(), this)"
      ];
      Line "}";
    ]
  in
  spaced [
    Line (sprintf {|// Original type: %s = { ... }|} name);
    Inline to_json;
    Inline to_json_string;
  ]

  let from_json_methods env name =
    let kt_class_name = class_name env name in
    let from_json =
      [
        Line (sprintf "fun fromJson(x: JsonElement): %s {"
                (single_esc kt_class_name));
        Block [
          Line "return Json.decodeFromJsonElement(serializer(), x)"
        ];
        Line "}";
      ]
    in
    let from_json_string =
      [
        Line (sprintf "fun fromJsonString(x: String): %s {"
                (single_esc kt_class_name));
        Block [
          Line "return Json.decodeFromString(serializer(), x)";
        ];
        Line "}";
      ]
    in
    spaced [
      Block from_json;
      Block from_json_string;
    ]

  let json_methods env name =
    Block [
      Inline (to_json_methods name);
      Line "";
      Line "companion object {";
      Inline (from_json_methods env name);
      Line "}";
    ]

let record env ~class_decorators loc name (fields : field list) an =
  let kt_class_name = class_name env name in
  let trans_meth = env.translate_inst_variable () in
  let fields =
    List.map (function
      | `Field x -> x
      | `Inherit _ -> (* expanded at loading time *) assert false)
      fields
  in
  (*
     Reorder fields with no-defaults first
  *)
  let fields =
    let no_default, with_default =
      List.partition has_no_class_inst_prop_default fields in
    no_default @ with_default
  in
  let inst_var_declarations =
    List.map (fun x -> Inline (inst_var_declaration env trans_meth x)) fields
  in
  [
    Inline class_decorators;
    Line (sprintf "data class %s(" kt_class_name);
    Block (spaced [Inline inst_var_declarations]);
    Line ") {";
    json_methods env name;
    Line "}";
  ]

(*
   A general-purpose wrapper that provides json-related methods for a type.
   This is used for tuples and for type aliases e.g. 'type foo = bar array'.

class Foo:
  def __init__(self, x: T):
    ...
  def to_json(self):
    ...
  def from_json(x):
    ...
  def to_json_string(self):
    ...
  def from_json_string(x):
    ...
*)
let alias_wrapper env ~class_decorators name type_expr =
  let kt_class_name = class_name env name in
  let value_type = type_name_of_expr env type_expr in
  [
    Inline class_decorators;
    Line (sprintf "data class %s(%s) {" kt_class_name (sprintf "val wrapped: %s" value_type));
    json_methods env name;
    Line "}";
  ]

let case_class env type_name
    (loc, orig_name, unique_name, an, opt_e) =
  let kt_class_name = class_name env type_name in
  let json_name = Atd.Json.get_json_cons orig_name an in
  let annotations = 
    if json_name <> orig_name then [
      Line "@Serializable";
      Line (sprintf "@SerialName(\"%s\")" json_name)
    ]
    else [Line "@Serializable"];
  in
  match opt_e with
  | None ->
      [
        Inline annotations;
        Line (sprintf "data object %s: %s()" (trans env unique_name) kt_class_name);
        Line (sprintf {|// Original type: %s = [ ... | %s | ... ]|}
                type_name orig_name);
      ]
  | Some e ->
      [
        Inline annotations;
        Line (sprintf "data class %s(val value: %s): %s()" 
                (trans env unique_name) (type_name_of_expr env e) kt_class_name);
        Line (sprintf {|// Original type: %s = [ ... | %s of ... | ... ]|}
                type_name orig_name);
      ]

let sum env ~class_decorators loc name cases =
  let cases =
    List.map (fun (x : variant) ->
      match x with
      | Variant (loc, (orig_name, an), opt_e) ->
          let unique_name = create_class_name env orig_name in
          (loc, orig_name, unique_name, an, opt_e)
      | Inherit _ -> assert false
    ) cases
  in
  let case_classes =
    List.map (fun x -> Inline (case_class env name x)) cases
    |> spaced
  in
  let kt_class_name = class_name env name in
  [
    Line "@Serializable";
    Line (sprintf "sealed class %s {" kt_class_name);
    Block (spaced [
      Line (sprintf {|// Original type: %s = [ ... ]|} name);
      Inline case_classes;
    ]);
    json_methods env name;
    Line "}";
  ]

let enum env loc name cases =
  let kt_class_name = class_name env name in
  let cases =
    List.map (fun (x : variant) ->
      match x with
      | Variant (loc, (orig_name, an), opt_e) ->
          let unique_name = create_class_name env orig_name in
          (loc, orig_name, unique_name, an, opt_e)
      | Inherit _ -> assert false
    ) cases
  in
  let cases0, cases1 =
    List.partition (fun (loc, orig_name, unique_name, an, opt_e) ->
      opt_e = None
    ) cases
  in
  let cases0_block =
    if cases0 <> [] then
      cases0 |> List.mapi (fun i (loc, orig_name, unique_name, an, opt_e) ->
      let json_name = Atd.Json.get_json_cons orig_name an in
      let comma = if i + 1 <> List.length cases0 then "," else ";" in
      Line (sprintf "%s%s" json_name comma);
    )
    else
      []
  in
  let cases1_block =
    if cases1 <> [] then
      error_at loc "enums with parameters are not supported"
    else
      []
  in
  [
    Line "@Serializable";
    Line (sprintf "enum class %s {" kt_class_name);
    Block (spaced [
      Inline cases0_block;
      Inline cases1_block;
    ]);
    json_methods env name;
    Line "}";
  ]

let get_class_decorators an = ["Serializable"]

let type_def env ((loc, (name, param, an), e) : A.type_def) : B.t =
  if param <> [] then
    not_implemented loc "parametrized type";
  let class_decorators =
    get_class_decorators an
    |> List.map (fun s -> Line ("@" ^ s))
  in
  let rec unwrap e =
    match e with
    | Sum (loc, cases, an) ->
        (match (Kotlin_annot.get_kotlin_sumtype_repr an) with
        | Sealed -> sum env ~class_decorators loc name cases
        | Enum -> enum env loc name cases)
    | Record (loc, fields, an) ->
        record env  ~class_decorators loc name fields an
    | Tuple _
    | List _
    | Option _
    | Nullable _
    | Name _ -> alias_wrapper env ~class_decorators name e
    | Shared _ -> not_implemented loc "cyclic references"
    | Wrap (loc, e, an) -> unwrap e
    | Tvar _ -> not_implemented loc "parametrized type"
  in
  unwrap e

let module_body env x =
  List.fold_left (fun acc (Type x) -> Inline (type_def env x) :: acc) [] x
  |> List.rev
  |> spaced

let definition_group ~atd_filename env
    (is_recursive, (items: A.module_body)) : B.t =
  [
    Inline (module_body env items);
  ]

(*
   Make sure that the types as defined in the atd file get a good name.
   For example, type 'foo' should become class 'Foo'.
   We do this because each case constructor of sum types will also
   translate to a class in the same namespace. For example,
   there may be a type like 'type bar = [ Foo | Bleep ]'.
   We want to ensure that the type 'foo' gets the name 'Foo' and that only
   later the case 'Foo' gets a lesser name like 'Foo_' or 'Foo2'.
*)
let reserve_good_class_names env (items: A.module_body) =
  List.iter
    (fun (Type (loc, (name, param, an), e)) -> ignore (class_name env name))
    items

let to_file ~atd_filename ~head (items : A.module_body) dst_path =
  let env = init_env () in
  reserve_good_class_names env items;
  let head = List.map (fun s -> Line s) head in
  let kotlin_defs =
    Atd.Util.tsort items
    |> List.map (fun x -> Inline (definition_group ~atd_filename env x))
  in
  Line (fixed_size_preamble atd_filename) :: Inline head :: kotlin_defs
  |> double_spaced
  |> Indent.to_file ~indent:4 dst_path

let run_file src_path =
  let src_name = Filename.basename src_path in
  let dst_name =
    (if Filename.check_suffix src_name ".atd" then
       Filename.chop_suffix src_name ".atd"
     else
       src_name) ^ ".kt"
    |> String.lowercase_ascii
  in
  let dst_path = dst_name in
  let full_module, _original_types =
    Atd.Util.load_file
      ~annot_schema
      ~expand:true (* monomorphization = eliminate parametrized type defs *)
      ~keep_builtins:true
      ~inherit_fields:true
      ~inherit_variants:true
      src_path
  in
  let full_module = Atd.Ast.use_only_specific_variants full_module in
  let (atd_head, atd_module) = full_module in
  let head = Kotlin_annot.get_kotlin_json_text (snd atd_head) in
  to_file ~atd_filename:src_name ~head atd_module dst_path
