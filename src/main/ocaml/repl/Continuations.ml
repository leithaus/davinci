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
sig
  type nominal
  type ('n, 'v) env
  type ('v, 't) cont =
      STOP
      | ARG of 't * (nominal, 'v) env * ('v, 't) cont
      | FUN of 'v * ('v, 't) cont
end

module type CONTINUATIONSFUNCTOR =
    functor (Nominal : NOMINALS) ->
      functor (Env : ENVIRONMENTS) ->
sig
  type nominal = Nominal.nominal
  type ('n, 'v) env = ('n, 'v) Env.env
  type ('v, 't) cont =
      STOP
      | ARG of 't * (nominal, 'v) env * ('v, 't) cont
      | FUN of 'v * ('v, 't) cont
end

module CONTINUATIONFUNCTOR : CONTINUATIONSFUNCTOR =
  functor ( Nominal : NOMINALS ) ->
    functor ( Env : ENVIRONMENTS ) ->
struct
  type nominal = Nominal.nominal
  type ('n, 'v) env = ('n, 'v) Env.env  
  type ( 'v, 't ) cont =
      STOP
      | ARG of 't * ( Nominal.nominal, 'v ) Env.env * ( 'v, 't ) cont
      | FUN of 'v * ( 'v, 't ) cont
end

