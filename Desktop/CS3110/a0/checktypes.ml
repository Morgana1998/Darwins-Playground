(* DO NOT EDIT THIS FILE *)

module type T = sig
  val valid_date : int -> string -> int -> bool
  val syr : int -> int 
  val nacci : int -> int -> int list
  val hours_worked : int
end

module M : T = Warmup