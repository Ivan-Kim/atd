(*
   Kotlin code generation for JSON support (no biniou support)
*)

(** Take ATD type definitions and translate them to Kotlin, writing
    them out to a file which should have the '.kt' extension. *)
val run_file : string -> unit
