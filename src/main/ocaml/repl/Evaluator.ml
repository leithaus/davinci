(* -*- mode: Tuareg;-*-  *)
(* Filename:    Evaluator.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Thu Aug 21 10:37:32 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Abscacao
open Environments
open Nominals
open Terms
open Values
open Continuations
open Exceptions
open Knot

module rec Eval :
  functor ( Value : VALUES ) -> 
    functor ( Env : ENVIRONMENTS ) -> 
      functor ( Cont : CONTINUATIONS ) ->
sig
  type ident = ReflectiveNominal.nominal 
  type term = ReflectiveTerm.term
  type value = Value( ReflectiveNominal )( ReflectiveTerm )( Env ).value
  type env = ( ident, value ) Env.env
  type ktn = ( value, term ) Cont( ReflectiveNominal )( Env ).cont
      
  val reduce : term -> env -> ktn -> value
end =
  functor ( Value : VALUES ) -> 
    functor ( Env : ENVIRONMENTS ) -> 
      functor ( Cont : CONTINUATIONS ) ->
struct
  type ident = ReflectiveNominal.nominal
  type term = ReflectiveTerm.term
  type value = Value( ReflectiveNominal )( ReflectiveTerm )( Env ).value
  type env = ( ident, value ) Env.env
  type ktn = ( value, term ) Cont( ReflectiveNominal )( Env ).cont
      
  let reduce t e k =
    match t with 
        ReflectiveTerm.Sequence( ts ) ->
          raise ( NotYetImplemented "Sequence" )
      | ReflectiveTerm.Application( op, actls ) ->
          raise ( NotYetImplemented "Application" )
      | ReflectiveTerm.Supposition( ptn, pterm, eterm ) ->
          raise ( NotYetImplemented "Supposition" )
      | ReflectiveTerm.Recurrence( ptn, pterm, eterm ) ->
          raise ( NotYetImplemented "Recurrence" )
      | ReflectiveTerm.Abstraction( ptn, eterm ) ->
          raise ( NotYetImplemented "Abstraction" )
      | ReflectiveTerm.Condition( test, tbranch, fbranch ) ->
          raise ( NotYetImplemented "Condition" )
      | ReflectiveTerm.Comprehension( bindings, eterm ) ->
          raise ( NotYetImplemented "Comprehension" )
      | ReflectiveTerm.Consolidation( bindings, eterm ) ->
          raise ( NotYetImplemented "Consolidation" )
      | ReflectiveTerm.Filtration( bindings, conditions, eterm ) ->
          raise ( NotYetImplemented "Filtration" )
      | ReflectiveTerm.Concentration( bindings, conditions, eterm ) ->
          raise ( NotYetImplemented "Concentration" )
      | ReflectiveTerm.Equation( lhs, rhs ) ->
          raise ( NotYetImplemented "Equation" )
      | ReflectiveTerm.ComparisonLT( lhs, rhs ) ->
          raise ( NotYetImplemented "ComparisonLT" )
      | ReflectiveTerm.ComparisonGT( lhs, rhs ) ->
          raise ( NotYetImplemented "ComparisonGT" )
      | ReflectiveTerm.ComparisonLTE( lhs, rhs ) ->
          raise ( NotYetImplemented "ComparisonLTE" )
      | ReflectiveTerm.ComparisonGTE( lhs, rhs ) ->
          raise ( NotYetImplemented "ComparisonGTE" )
      | ReflectiveTerm.Reflection( v ) ->
          raise ( NotYetImplemented "Reflection" )
      | ReflectiveTerm.Acquisition -> 
          raise ( NotYetImplemented "Acquisition" )
      | ReflectiveTerm.Suspension( pterm, eterm ) -> 
          raise ( NotYetImplemented "Suspension" )
      | ReflectiveTerm.Release( pterm, eterm ) -> 
          raise ( NotYetImplemented "Release" )
      | ReflectiveTerm.InnerSuspension( pterm, eterm ) -> 
          raise ( NotYetImplemented "InnerSuspension" )
      | ReflectiveTerm.Calculation( aterm ) -> 
          raise ( NotYetImplemented "Calculuation" )
end
(* This gives a simple and effective form of reflection for quasiquote *)
and ReflectiveNominal : NOMINALS = NOMINAL( ReflectiveTerm )
and ReflectiveTerm : TERMS = TERM( ReflectiveNominal ) 
