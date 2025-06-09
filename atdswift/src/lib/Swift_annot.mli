(**
   Swift-specific ATD annotations.

   This interface serves as a reference of which Swift-specific
   ATD annotations are supported. Atdswift also honors JSON-related annotations
   defined in [Atd.Json].
*)

(** Extract ["42"] from [<swift default="42">].
    The provided default must be a well-formed Swift immutable expression.
*)
val get_swift_default : Atd.Annot.t -> string option

(** Whether an association list of ATD type [(string * foo) list]
    must be represented in Swift as a list of pairs or as a dictionary.
    This is independent of the JSON representation.
*)
type assoc_repr =
  | List
  | Dict

(** Inspect annotations placed on lists of pairs such as
    [(string * foo) list <python repr="dict">].
    Permissible values for the [repr] field are ["dict"] and ["list"].
    The default is ["list"].
*)
val get_swift_assoc_repr : Atd.Annot.t -> assoc_repr

(** Returns the list of class decorators as specified by the user without
    [@] e.g. [<swift decorator="foo" decorator="bar(baz)">]
    gives [["foo"; "bar(baz)"]]. *)
val get_swift_decorators : Atd.Annot.t -> string list

(** Returns text the user wants to be inserted at the beginning of the
    Swift file such as imports. *)
val get_swift_json_text : Atd.Annot.t -> string list
