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
  type ('n, 'a) env
  val empty : ('n, 'a) env
  val extend : 'n * 'a * ('n, 'a) env -> ('n, 'a) env
  val sum : ('n, 'a) env -> ('n, 'a) env -> ('n, 'a) env
  val lookup : 'n * ('n, 'a) env -> 'a option
end

(* module HashtblEnv ( Nominal : NOMINALS ) = *)
(* struct *)
(*   module HNoms = HashedNominals( Nominal ) *)
(*   module Table = Hashtbl.Make( HNoms ) *)
(*   type ('n, 'a) env = 'a Table.t *)
(*   let empty = raise ( NotYetImplemented "empty" ) *)
(*   let extend tpl = *)
(*     match tpl with *)
(*         ( n, a, env ) -> ( Table.add env n a ) *)
(*   let lookup tpl = *)
(*     match tpl with *)
(*         ( n, env ) -> ( Table.find env n ) *)
(* end *)

module ListEnv : ENVIRONMENTS =
struct
  type ('n, 'a) env = ('n * 'a) list
  let empty : ( 'n * 'a ) list = []
  let extend tpl = 
    match tpl with
        ( n, a, env ) -> ( n, a ) :: env
  let sum e1 e2 = ( List.append e1 e2 )
  let lookup tpl = 
    match tpl with
        ( n, env ) ->
          try
            let ( _, a ) =
              (List.find
                  ( fun ( p ) -> match p with ( n, a ) -> true )
                  env ) in
              ( Some a )
          with
              Not_found -> None
end
