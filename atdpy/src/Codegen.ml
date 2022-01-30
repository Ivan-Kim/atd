(*
   Python code generation for JSON support (no biniou support)

  Takes the contents of a .atd file and translates it to a .py file.
*)

open Printf
open Atd.Ast
open Indent
module A = Atd.Ast
module B = Indent

(* Mutable environment holding hash tables and such to avoid
   naming conflicts. *)
type env = Unique_name.t

(* Translate an identifier found in an ATD file to an available Python
   identifier. *)
let trans env id =
  Unique_name.translate env id

type quote_kind = Single | Double

(* Escape a string fragment to be placed in single quotes or double quotes.
   https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals
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

let init_env () : env =
  Unique_name.init
    ~reserved_identifiers:[
      (* Keywords
         https://docs.python.org/3/reference/lexical_analysis.html#keywords
      *)
      "False"; "await"; "else"; "import"; "pass";
      "None"; "break"; "except"; "in"; "raise";
      "True"; "class"; "finally"; "is"; "return";
      "and"; "continue"; "for"; "lambda"; "try";
      "as"; "def"; "from"; "nonlocal"; "while";
      "assert"; "del"; "global"; "not"; "with";
      "async"; "elif"; "if"; "or"; "yield";

      (* Soft keywords
         https://docs.python.org/3/reference/lexical_analysis.html#soft-keywords
      *)
      "match"; "case"; "_";
    ]
    ~reserved_prefixes:["atd_"; "_atd_"]
    ~safe_prefix:"x_"

let fixed_size_preamble atd_filename =
  sprintf {|"""Generated by atdpy from %s.

This implements classes for the types defined in '%s', providing
methods and functions to convert data from/to JSON.
"""

from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

import json


def atd_missing_field(type_name: str, json_field_name: str):
    raise ValueError(f"missing field '{json_field_name}'"
                     " in JSON object of type '{type_name}'")


def atd_type_mismatch(expected_type: str, json_value: Any):
    value_str = str(json_value)
    if len(value_str) > 200:
        value_str = value_str[:200] + '…'

    raise ValueError(f"incompatible JSON value where"
                     " type '{expected_type}' was expected: '{value_str}'")
|}
    atd_filename
    atd_filename

let not_implemented loc msg =
  A.error_at loc ("not implemented in atdpy: " ^ msg)

let todo hint =
  failwith ("TODO: " ^ hint)

let rec spaced (xs : B.node list) : B.node list =
  match List.filter (fun x -> not (B.is_empty_node x)) xs with
  | []
  | [_] as xs -> xs
  | a :: rest -> a :: Line "" :: spaced rest

(* Map ATD built-in types to built-in mypy types *)
let map_type_name (name : string) =
  match name with
  | "unit" -> "Any"
  | "bool" -> "bool"
  | "int" -> "int"
  | "float" -> "float"
  | "string" -> "str"
  | "abstract" -> (* not supported *) "Any"
  | user_defined -> user_defined

let rec type_name_of_expr env (e : type_expr) : string =
  match e with
  | Sum (loc, _, _) -> not_implemented loc "inline sum types"
  | Record (loc, _, _) -> not_implemented loc "inline records"
  | Tuple (loc, xs, an) ->
      let type_names =
        xs
        |> List.map (fun (loc, x, an) -> type_name_of_expr env x)
      in
      sprintf "Tuple[%s]" (String.concat ", " type_names)
  | List (loc, e, an) -> sprintf "List[%s]" (type_name_of_expr env e)
  | Option (loc, e, an) -> sprintf "Optional[%s]" (type_name_of_expr env e)
  | Nullable (loc, e, an) -> type_name_of_expr env e
  | Shared (loc, e, an) -> not_implemented loc "shared"
  | Wrap (loc, e, an) -> todo "wrap"
  | Name (loc, (loc2, name, []), an) ->
      let py_name = map_type_name name in
      trans env py_name
  | Name (loc, _, _) -> not_implemented loc "parametrized types"
  | Tvar (loc, _) -> not_implemented loc "type variables"

let field_as_param env ((loc, (name, kind, an), e) : simple_field) =
  let type_name = type_name_of_expr env e in
  [
    Line (sprintf "%s: %s," (trans env name) (trans env type_name))
  ]

let property_var_name field_name =
  "_atd_" ^ field_name

let field_init env ((loc, (name, kind, an), e) : simple_field) =
  let var_name = property_var_name name in
  [
    Line (sprintf "self.%s = %s" var_name (trans env name))
  ]

let property_definition env ((loc, (name, kind, an), e) : simple_field) =
  [
    Line "@property";
    Line (sprintf "def %s(self):" (trans env name));
    Block [
      Line (sprintf "return self.%s" (property_var_name name))
    ]
  ]

let construct_json_field env ((loc, (name, kind, an), e) : simple_field) =
  [
    Line (sprintf "'%s': self.%s,"
            (Atdgen_emit.Json.get_json_fname name an |> single_esc)
            (property_var_name name))
  ]

(* Call the checker or converter from the json node to the desired
   python representation. The generated code must convert the expression
   json_value and assign it to the variable py_var.
*)
let convert_from_json
    env (json_value : string) (left_value : string) (e : type_expr) =
  (* TODO: maybe ensure that all type expressions are just names, i.e.
     list/option have they own name and from_json/to_json functions. *)
  [
    Line (sprintf "%s = %s  # TODO" left_value json_value)
  ]

let initialize_field_from_json
    env py_class_name ((loc, (name, kind, an), e) : simple_field) =
  let json_name = Atdgen_emit.Json.get_json_fname name an in
  let json_value = sprintf "x['%s']" (single_esc json_name) in
  let left_value = trans env name in
  [
    Line (sprintf "if '%s' in x:" (single_esc json_name));
    Block (convert_from_json env json_value left_value e);
    Line "else:";
    Block [
      Line (sprintf "atd_missing_field('%s', '%s')"
              (single_esc py_class_name)
              (single_esc json_name))
    ]
  ]

let class_arg env ((loc, (name, kind, an), e) : simple_field) =
  [
    Line (sprintf "%s," (trans env name))
  ]

let record env loc class_name (fields : field list) an =
  let py_class_name = trans env class_name in
  (* Local environment *)
  let env = Unique_name.copy env in
  let fields =
    List.map (function
      | `Field x -> x
      | `Inherit _ -> (* expanded at loading time *) assert false)
      fields
  in
  let init_params =
    List.map (fun x -> Inline (field_as_param env x)) fields
  in
  let init_body =
    List.map (fun x -> Inline (field_init env x)) fields
  in
  let init =
    [
      Line "def __init__(";
      Block [
        Line "self,";
        Inline init_params;
      ];
      Line "):";
      Block init_body;
    ]
  in
  let properties =
    List.map (fun x -> Inline (property_definition env x)) fields
    |> spaced
  in
  let json_object_body =
    List.map (fun x -> Inline (construct_json_field env x)) fields in
  let field_init_from_json =
    List.map (fun x ->
      Inline (initialize_field_from_json env py_class_name x)) fields in
  let class_arguments =
    List.map (fun x -> Inline (class_arg env x)) fields in
  let from_json =
    [
      Line "@classmethod";
      Line "def from_json(cls, x: Any):";
      Block [
        Line "if isinstance(x, dict):";
        Block field_init_from_json;
        Line "else:";
        Block [
          Line (sprintf "atd_type_mismatch('%s', x)"
                  (single_esc py_class_name))
        ];
        Line "return cls(";
        Block class_arguments;
        Line ")";
      ]
    ]
  in
  let to_json =
    [
      Line "def to_json(self) -> Any:";
      Block [
        Line "return {";
        Block json_object_body;
        Line "}"
      ]
    ]
  in
  let from_json_string =
    [
      Line "@classmethod";
      Line "def from_json_string(cls, x: str):";
      Block [
        Line "return cls.from_json(json.loads(x))"
      ]
    ]
  in
  let to_json_string =
    [
      Line "def to_json_string(self) -> str:";
      Block [
        Line "return json.dumps(self.to_json())"
      ]
    ]
  in
  [
    Line (sprintf "class %s:" py_class_name);
    Block (spaced [
      Inline init;
      Inline properties;
      Inline from_json;
      Inline to_json;
      Inline from_json_string;
      Inline to_json_string;
    ])
  ]

let type_def env ((loc, (name, param, an), e) : A.type_def) : B.t =
  if param <> [] then
    not_implemented loc "parametrized type";
  let rec unwrap e =
    match e with
    | Sum (loc, xs, an) -> todo "sum type"
    | Record (loc, fields, an) -> record env loc name fields an
    | Tuple (loc, xs, an) -> todo "tuple"
    | List (loc, e, an) -> todo "list/array"
    | Option (loc, e, an) -> todo "option"
    | Nullable (loc, e, an) -> todo "nullable"
    | Shared (loc, e, an) -> not_implemented loc "cyclic references"
    | Wrap (loc, e, an) -> unwrap e
    | Name (loc, type_inst, an) -> todo "atomic type"
    | Tvar (loc, s) -> not_implemented loc "parametrized type"
  in
  unwrap e

let module_body env x =
  List.fold_left (fun acc (Type x) -> Inline (type_def env x) :: acc) [] x
  |> List.rev

let to_file ~atd_filename (x : A.module_body) dst_path =
  let env = init_env () in
  let tree = [
    Line (fixed_size_preamble atd_filename);
    Inline (module_body env x);
  ]
  in
  Indent.to_file ~indent:4 dst_path tree
