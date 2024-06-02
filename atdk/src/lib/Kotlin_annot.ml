(*
   ATD annotations to be interpreted specifically by atdk.

   Atdk also honors json-related annotations defined in Atd.Json.
*)

type assoc_repr =
  | List
  | Map

type sumtype_repr =
  | Sealed
  | Enum

let get_kotlin_default an : string option =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["kotlin"]
      ~field:"default"
      an

let get_kotlin_assoc_repr an : assoc_repr =
  Atd.Annot.get_field
    ~parse:(function
      | "list" -> Some List
      | "map" -> Some Map
      | _ -> None
    )
    ~default:List
    ~sections:["kotlin"]
    ~field:"repr"
    an

let get_kotlin_sumtype_repr an : sumtype_repr =
  Atd.Annot.get_field
    ~parse:(function
      | "sealed" -> Some Sealed
      | "enum" -> Some Enum
      | _ -> None
    )
    ~default:Sealed
    ~sections:["kotlin"]
    ~field:"repr"
    an

(* imports etc. *)
let get_kotlin_text an : string list =
  Atd.Annot.get_fields
    ~parse:(fun s -> Some s)
    ~sections:["kotlin"]
    ~field:"text"
    an

let get_kotlin_json_text an : string list =
  get_kotlin_text an
  @ Atd.Annot.get_fields
    ~parse:(fun s -> Some s)
    ~sections:["kotlin"]
    ~field:"json_kt.text"
    an

let get_kotlin_module an : string option =
  let o = Atd.Annot.get_opt_field
    ~parse:(fun s -> Some s)
    ~sections:["kotlin"]
    ~field:"module"
    an
  in
  match o with
  | Some s -> Some s
  | None ->
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["kotlin"]
      ~field:"from" 
      an
      
let get_kotlin_t an default: string =
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default:default
    ~sections:["kotlin"]
    ~field:"t"
    an