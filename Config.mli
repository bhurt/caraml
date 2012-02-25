
(* The maximum number of arguments a function can take, or which can
 * be applied in one call.
 *)
val max_args : int;;

(* The name of a function to directly call it. *)
val direct_name : Common.Var.t -> string;;

(* The name of a function to apply it. *)
val apply_name : Common.Var.t -> string;;
