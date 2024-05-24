(**
   Kotlin-specific ATD annotations.

   This interface serves as a reference of which Kotlin-specific
   ATD annotations are supported. Atdk also honors JSON-related annotations
   defined in [Atd.Json].
*)

(** Extract ["42"] from [<kotlin default="42">].
    The provided default must be a well-formed Kotlin immutable expression.
*)
val get_kotlin_default : Atd.Annot.t -> string option

(** Whether an association list of ATD type [(string * foo) list]
    must be represented in Kotlin as a list of pairs or as a dictionary.
    This is independent of the JSON representation.
*)
type assoc_repr =
  | List
  | Map

(** Inspect annotations placed on lists of pairs such as
    [(string * foo) list <kotlin repr="map">].
    Permissible values for the [repr] field are ["map"] and ["list"].
    The default is ["list"].
*)
val get_kotlin_assoc_repr : Atd.Annot.t -> assoc_repr

(** Returns text the user wants to be inserted at the beginning of the
    Kotlin file such as imports. *)
val get_kotlin_json_text : Atd.Annot.t -> string list
