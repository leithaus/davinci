(* -*- mode: Tuareg;-*-  *)
(* Filename:    Continuations.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Thu Aug 21 23:57:45 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Environments
open Nominals

module type CONTINUATIONS =
    functor (Nominal : NOMINALS) ->
      functor (Env : ENVIRONMENTS) ->
sig
  type ('v, 't) cont =
      STOP
      | ARG of 't * (Nominal.nominal, 'v) Env.env * ('v, 't) cont
      | FUN of 'v * ('v, 't) cont
end

module CONTINUATION ( Nominal : NOMINALS ) ( Env : ENVIRONMENTS ) =
struct
  type ( 'v, 't ) cont =
      STOP
      | ARG of 't * ( Nominal.nominal, 'v ) Env.env * ( 'v, 't ) cont
      | FUN of 'v * ( 'v, 't ) cont
end

