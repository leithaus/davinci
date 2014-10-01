(* -*- mode: Tuareg;-*-  *)
(* Filename:    Showoff.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Wed Oct  1 12:29:15 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Showcacao
open Terms
open Values
open Exceptions

module type SHOWOFF =
sig
  type term
  type arithemetic_term
  type pattern
  type binding
  type variation
  type value
  type ground
  type environment
  type continuation
  type meta_continuation
      
  val show_term : term -> showable
  val show_arithmetic_term : arithemetic_term -> showable
  val show_binding : binding -> showable
  val show_pattern : pattern -> showable
  val show_variation : variation -> showable
  val show_value : value -> showable
  val show_ground : ground -> showable
  val show_env : environment -> showable
  val show_k : continuation -> showable
  val show_mk : meta_continuation -> showable
end  

module type SHOWOFFFUNCTOR =
  functor( Terms : TERMS ) ->
    functor( Values : VALUES ) ->
sig
  type term = Terms.term
  type arithemetic_term = Terms.arithmeticTerm
  type pattern = Terms.pattern
  type binding = Terms.binding      
  type variation = Terms.variation
  type value = Values.value
  type ground = Values.ground
  type environment = Values.v_env
  type continuation = Values.v_ktn
  type meta_continuation = Values.v_meta_ktn

  val show_term : term -> showable
  val show_arithmetic_term : arithemetic_term -> showable
  val show_binding : binding -> showable
  val show_pattern : pattern -> showable
  val show_variation : variation -> showable
  val show_value : value -> showable
  val show_ground : ground -> showable
  val show_env : environment -> showable
  val show_k : continuation -> showable
  val show_mk : meta_continuation -> showable
end  

module ShowOffFunctor : SHOWOFFFUNCTOR =
  functor( Terms : TERMS ) ->
    functor( Values : VALUES ) ->
struct
  type term = Terms.term
  type arithemetic_term = Terms.arithmeticTerm
  type pattern = Terms.pattern
  type binding = Terms.binding      
  type variation = Terms.variation
  type value = Values.value
  type ground = Values.ground
  type environment = Values.v_env
  type continuation = Values.v_ktn
  type meta_continuation = Values.v_meta_ktn

  let show_term t =
    raise ( NotYetImplemented "show_term" )
  let show_arithmetic_term a =
    raise ( NotYetImplemented "show_arithmetic_term" )
  let show_binding b = 
    raise ( NotYetImplemented "show_binding" )
  let show_pattern p =
    raise ( NotYetImplemented "show_pattern" )
  let show_variation v =
    raise ( NotYetImplemented "show_variation" )
  let show_value v =
    raise ( NotYetImplemented "show_value" )
  let show_ground g =
    raise ( NotYetImplemented "show_ground" )
  let show_env e = 
    raise ( NotYetImplemented "show_env" )
  let show_k k =
    raise ( NotYetImplemented "show_k" )
  let show_mk mk =
    raise ( NotYetImplemented "show_mk" )
end
