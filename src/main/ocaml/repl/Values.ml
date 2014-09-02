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

module type VALUES =
sig
  type ident 
  type term
  type pattern
  type ('n, 'v) environment
  type value =
      Ground of ground
      | Closure of pattern * term * env
      | BOTTOM
      | UNIT
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and env = (ident, value) environment
end

module VALUE : VALUES =
struct
  type ident 
  type term
  type pattern
  type ('n, 'v) environment
  type value =
      Ground of ground
      | Closure of pattern * term * env
      | BOTTOM
      | UNIT
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and env = (ident, value) environment
end

module type VALUESFUNCTOR =
  functor ( Nominal : NOMINALS ) ->
    functor ( Term : TERMS ) ->
      functor ( Env : ENVIRONMENTS ) ->
sig
  type ident = Nominal.nominal 
  type term = Term.term
  type pattern = Term.pattern 
  type ('n, 'v) environment = ('n, 'v) Env.env
  type value = 
      Ground of ground
      | Closure of pattern * term * env 
      | BOTTOM
      | UNIT 
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and env = ( ident, value ) Env.env 
end
(*   with type ident = Nominal.nominal *)
(* with type term = Term.term *)
(* with type pattern = Term.pattern *)
(* with type ('n, 'v) environment = ('n, 'v) Env.env *)

module VALUEFUNCTOR : VALUESFUNCTOR =
  functor ( Nominal : NOMINALS ) ->
    functor ( Term : TERMS ) ->
      functor ( Env : ENVIRONMENTS ) ->
struct
  type ident = Nominal.nominal 
  type term = Term.term
  type pattern = Term.pattern
  type ('n, 'v) environment = ('n, 'v) Env.env
  type value =
      Ground of ground
      | Closure of pattern * term * env
      | BOTTOM
      | UNIT
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of term
  and env = ( ident, value ) Env.env
end



