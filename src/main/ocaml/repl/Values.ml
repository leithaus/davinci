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
  functor (Nominal : NOMINALS) ->
    functor (Term : TERMS) ->
      functor (Env : ENVIRONMENTS) ->
sig
  type ident = Nominal.nominal
  type term = Term.term
  type value =
      Ground of ground
      | Closure of ident list * term * env
      | BOTTOM
      | UNIT
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of Term.term
  and env = (ident, value) Env.env
end

module VALUE ( Nominal : NOMINALS ) ( Term : TERMS ) ( Env : ENVIRONMENTS ) =
struct
  type ident = Nominal.nominal
  type term = Term.term                
  type value =
      Ground of ground
      | Closure of ident list * term * env
      | BOTTOM
  and ground =
      Boolean of bool
      | String of string
      | Integer of int
      | Double of float
      | Reification of Term.term
  and env = ( ident, value ) Env.env
end

