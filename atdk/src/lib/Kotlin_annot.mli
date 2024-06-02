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

(** Whether a sum type must be represented in Kotlin as sealed class or as an enum.
    This is independent of the JSON representation.
*)
type sumtype_repr =
  | Sealed
  | Enum

(** Inspect annotations placed on lists of pairs such as
    [(string * foo) list <kotlin repr="map">].
    Permissible values for the [repr] field are ["map"] and ["list"].
    The default is ["list"].
*)
val get_kotlin_assoc_repr : Atd.Annot.t -> assoc_repr

(** Inspection of annotations placed on sum types such as
    [type foo = A | B | C <kotlin repr="enum">].
    Permissible values for the [repr] field are ["enum"] and ["sealed"].
    The default is ["sealed"].
*)
val get_kotlin_sumtype_repr : Atd.Annot.t -> sumtype_repr

(** Returns text the user wants to be inserted at the beginning of the
    Kotlin file such as imports. *)
val get_kotlin_json_text : Atd.Annot.t -> string list

val get_kotlin_module : Atd.Annot.t -> string option

val get_kotlin_t : Atd.Annot.t -> string -> string
