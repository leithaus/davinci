(* -*- mode: Tuareg;-*-  *)
(* Filename:    Environments.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Thu Aug 21 23:54:49 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Nominals
open Exceptions

module type ENVIRONMENTS =
sig
  type ('n, 'a) map
  val empty : ('n, 'a) map
  val extend : 'n * 'a * ('n, 'a) map -> ('n, 'a) map
  val sum : ('n, 'a) map -> ('n, 'a) map -> ('n, 'a) map
  val lookup : 'n * ('n, 'a) map -> 'a option
end

(* module HashtblEnv ( Nominal : NOMINALS ) = *)
(* struct *)
(*   module HNoms = HashedNominals( Nominal ) *)
(*   module Table = Hashtbl.Make( HNoms ) *)
(*   type ('n, 'a) map = 'a Table.t *)
(*   let empty = raise ( NotYetImplemented "empty" ) *)
(*   let extend tpl = *)
(*     match tpl with *)
(*         ( n, a, map ) -> ( Table.add map n a ) *)
(*   let lookup tpl = *)
(*     match tpl with *)
(*         ( n, map ) -> ( Table.find map n ) *)
(* end *)

module ListEnv : ENVIRONMENTS =
struct
  type ('n, 'a) map = ('n * 'a) list
  let empty : ( 'n * 'a ) list = []
  let extend tpl = 
    match tpl with
        ( n, a, m ) -> ( n, a ) :: m
  let sum e1 e2 = ( List.append e1 e2 )
  let lookup tpl = 
    match tpl with
        ( n, m ) ->
          try
            let ( _, a ) =
              (List.find
                  ( fun ( p ) -> match p with ( n, a ) -> true )
                  m ) in
              ( Some a )
          with
              Not_found -> None
end
