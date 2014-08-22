(* -*- mode: Tuareg;-*-  *)
(* Filename:    Nominals.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Thu Aug 21 23:55:54 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

module type NOMINALS =
sig
  type symbol
  type nominal
  val comparator : nominal -> nominal -> bool
  val toString : nominal -> string
  val fresh : unit -> nominal
end 

