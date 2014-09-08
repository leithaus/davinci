(* -*- mode: Tuareg;-*-  *)
(* Filename:    Symbols.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Sun Sep  7 17:13:02 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

module type SYMBOLS =
sig
  type url = string
  type opaque = string
  type debruijn = int * int
      
  type symbol =
      URL of url
      | Opaque of opaque
      | Debruijn of debruijn
end

module Symbols : SYMBOLS =
struct
  type url = string
  type opaque = string
  type debruijn = int * int
      
  type symbol =
      URL of url
      | Opaque of opaque
      | Debruijn of debruijn
end
