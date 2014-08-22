(* -*- mode: Tuareg;-*-  *)
(* Filename:    Environments.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Thu Aug 21 23:54:49 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

module type ENVIRONMENTS =
sig
  type ('n, 'a) env
  val empty : ('n, 'a) env
  val extend : 'n * 'a * ('n, 'a) env -> ('n, 'a) env
  val lookup : 'n * ('n, 'a) env -> 'a
end

