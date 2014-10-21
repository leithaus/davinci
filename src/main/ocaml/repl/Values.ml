(* -*- mode: Tuareg;-*-  *)
(* Filename:    Values.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Thu Aug 21 23:57:19 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Environments
open Nominals
open Terms
open Continuations

module type VALUES =
sig
  type ident 
  type term
  type pattern
  type prompt
  type ('n, 'v) environment
  type ('v, 't) continuation
  type ('v, 't) meta_continuation
  type value =
      Ground of ground
      | Closure of pattern * term * v_env
      | Cont of v_ktn
      | MCont of v_meta_ktn
      | Prompt of v_prompt
      | BOTTOM
      | UNIT
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and v_prompt =
      P of prompt
  and v_env =
      Env of (ident, value) environment
  and v_ktn =
      K of ( value, term ) continuation
  and v_meta_ktn =
      MK of ( value, term ) meta_continuation
end

module VALUE : VALUES =
struct
  type ident 
  type term
  type pattern
  type prompt
  type ('n, 'v) environment
  type ('v, 't) continuation
  type ('v, 't) meta_continuation
  type value =
      Ground of ground
      | Closure of pattern * term * v_env
      | Cont of v_ktn
      | MCont of v_meta_ktn
      | Prompt of v_prompt
      | BOTTOM
      | UNIT
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and v_prompt =
      P of prompt
  and v_env =
      Env of (ident, value) environment  
  and v_ktn =
      K of ( value, term ) continuation
  and v_meta_ktn =
      MK of ( value, term ) meta_continuation
end

module type VALUESFUNCTOR =
  functor ( Nominal : NOMINALS ) ->
    functor ( Term : TERMS ) ->
      functor ( Env : ENVIRONMENTS ) ->
        functor ( K : CONTINUATIONS ) ->
sig
  type ident = Nominal.nominal 
  type term = Term.term
  type pattern = Term.pattern 
  type prompt = K.prompt
  type ('n, 'v) environment = ('n, 'v) Env.map
  type ('v, 't) continuation = ( 'v, 't ) K.cont
  type ('v, 't) meta_continuation = ( 'v, 't ) K.meta_cont
  type value = 
      Ground of ground
      | Closure of pattern * term * v_env
      | Cont of v_ktn
      | MCont of v_meta_ktn
      | Prompt of v_prompt
      | BOTTOM
      | UNIT 
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and v_prompt =
      P of prompt
  and v_env =
      Env of ( ident, value ) Env.map
  and v_ktn =
      K of ( value, term ) continuation
  and v_meta_ktn =
      MK of ( value, term ) meta_continuation
end

module VALUEFUNCTOR : VALUESFUNCTOR =
  functor ( Nominal : NOMINALS ) ->
    functor ( Term : TERMS ) ->
      functor ( Env : ENVIRONMENTS ) ->
        functor ( K : CONTINUATIONS ) ->
struct
  type ident = Nominal.nominal 
  type term = Term.term
  type pattern = Term.pattern
  type prompt = K.prompt
  type ('n, 'v) environment = ('n, 'v) Env.map
  type ('v, 't) continuation = ( 'v, 't ) K.cont
  type ('v, 't) meta_continuation = ( 'v, 't ) K.meta_cont
  type value =
      Ground of ground
      | Closure of pattern * term * v_env
      | Cont of v_ktn
      | MCont of v_meta_ktn
      | Prompt of v_prompt
      | BOTTOM
      | UNIT
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and v_prompt =
      P of prompt
  and v_env = 
      Env of ( ident, value ) Env.map
  and v_ktn =
      K of ( value, term ) continuation
  and v_meta_ktn =
      MK of ( value, term ) meta_continuation
end



