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
  type ('n, 'v) environment
  type ('v, 't) continuation
  type value =
      Ground of ground
      | Closure of pattern * term * v_env
      | Cont of v_ktn
      | BOTTOM
      | UNIT
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and v_env =
      Env of (ident, value) environment
  and v_ktn =
      K of ( value, term ) continuation
end

module VALUE : VALUES =
struct
  type ident 
  type term
  type pattern
  type ('n, 'v) environment
  type ('v, 't) continuation
  type value =
      Ground of ground
      | Closure of pattern * term * v_env
      | Cont of v_ktn
      | BOTTOM
      | UNIT
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and v_env =
      Env of (ident, value) environment  
  and v_ktn =
      K of ( value, term ) continuation
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
  type ('n, 'v) environment = ('n, 'v) Env.map
  type ('v, 't) continuation = ( 'v, 't ) K.cont
  type value = 
      Ground of ground
      | Closure of pattern * term * v_env
      | Cont of v_ktn
      | BOTTOM
      | UNIT 
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and v_env =
      Env of ( ident, value ) Env.map
  and v_ktn =
      K of ( value, term ) continuation
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
  type ('n, 'v) environment = ('n, 'v) Env.map
  type ('v, 't) continuation = ( 'v, 't ) K.cont
  type value =
      Ground of ground
      | Closure of pattern * term * v_env
      | Cont of v_ktn
      | BOTTOM
      | UNIT
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and v_env = 
      Env of ( ident, value ) Env.map
  and v_ktn =
      K of ( value, term ) continuation
end



