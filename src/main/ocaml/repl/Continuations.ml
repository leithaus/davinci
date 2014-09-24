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
  type ('n, 'v) k_env  

  type prompt = int

  type ('v, 't) cont =
      STOP
      | ARG of 't * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | FUN of 'v * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | PUSHPROMPT of 't * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | WITHSUBCONT of 't * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | PUSHSUBCONT of 't * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | FUNPUSHPROMPT of (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | FUNWITHSUBCONT of 'v * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | FUNPUSHSUBCONT of 'v * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt          
  (* The type of contexts *)
  and ('v, 't) k_ctxt =
      Prompt of prompt
      | K of ('v, 't) cont
  (* The type of meta-continuations *)
  and ('v, 't) meta_cont = ( ('v, 't) k_ctxt ) list
end

module type CONTINUATIONSFUNCTOR =
    functor (Nominal : NOMINALS) ->
      functor (Env : ENVIRONMENTS) ->
sig
  type nominal = Nominal.nominal  
  type ('n, 'v) k_env = ('n, 'v) Env.map

  type prompt = int

  type ('v, 't) cont =
      STOP
      | ARG of 't * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | FUN of 'v * ('v, 't) cont * ('v, 't) meta_cont * prompt          
      | PUSHPROMPT of 't * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | WITHSUBCONT of 't * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | PUSHSUBCONT of 't * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | FUNPUSHPROMPT of (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | FUNWITHSUBCONT of 'v * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | FUNPUSHSUBCONT of 'v * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
  (* The type of contexts *)
  and ('v, 't) k_ctxt =
      Prompt of prompt
      | K of ('v, 't) cont
  (* The type of meta-continuations *)
  and ('v, 't) meta_cont = ( ('v, 't) k_ctxt ) list
end

module CONTINUATIONFUNCTOR : CONTINUATIONSFUNCTOR =
  functor ( Nominal : NOMINALS ) ->
    functor ( Env : ENVIRONMENTS ) ->
struct  
  type nominal = Nominal.nominal  
  type ('n, 'v) k_env = ('n, 'v) Env.map

  type prompt = int

  type ( 'v, 't ) cont =
      STOP
      | ARG of 't * ( Nominal.nominal, 'v ) k_env * ( 'v, 't ) cont * ('v, 't) meta_cont * prompt
      | FUN of 'v * ( 'v, 't ) cont * ('v, 't) meta_cont * prompt
      | PUSHPROMPT of 't * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | WITHSUBCONT of 't * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | PUSHSUBCONT of 't * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | FUNPUSHPROMPT of (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | FUNWITHSUBCONT of 'v * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
      | FUNPUSHSUBCONT of 'v * (nominal, 'v) k_env * ('v, 't) cont * ('v, 't) meta_cont * prompt
  (* The type of contexts *)
  and ('v, 't) k_ctxt =
      Prompt of prompt
      | K of ('v, 't) cont
  (* The type of meta-continuations *)
  and ('v, 't) meta_cont = ( ('v, 't) k_ctxt ) list
end

