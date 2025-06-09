(*
   Swift code generation for JSON support (no biniou support)
*)

(** Take ATD type definitions and translate them to Swift, writing
    them out to a file which should have the '.swift' extension. *)
val run_file : string -> unit
