(* -*- mode: Tuareg;-*-  *)
(* Filename:    Evaluator.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Thu Aug 21 10:37:32 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Abscacao

module type ENVIRONMENTS =
sig
  type ('n, 'a) env
  val empty : ('n, 'a) env
  val extend : 'n * 'a * ('n, 'a) env -> ('n, 'a) env
  val lookup : 'n * ('n, 'a) env -> 'a
end

module type NOMINALS =
sig
  type symbol
  type nominal
  val comparator : nominal -> nominal -> bool
  val toString : nominal -> string
  val fresh : unit -> nominal
end 

module type TERMS =
sig
  type var 
  type term =
      Sequence of term list
      | Application of term * term list
      | Supposition of pattern * term * term
      | Recurrence of pattern * term * term
      | Abstraction of pattern * term
      | Condition of term * term * term
      | Comprehension of binding list * term
      | Consolidation of binding list * term
      | Filtration of binding list * pattern list * term
      | Concentration of binding list * pattern list * term
      | Equation of term * term
      | ComparisonLT of term * term
      | ComparisonGT of term * term
      | ComparisonLTE of term * term
      | ComparisonGTE of term * term
      | Reflection of var
      | Acquisition
      | Suspension of term * term
      | Release of term * term
      | InnerSuspension of term * term
      | Calculation of arithmeticTerm
  and arithmeticTerm =
      Division of arithmeticTerm * arithmeticTerm
      | Addition of arithmeticTerm * arithmeticTerm
      | Multiplication of arithmeticTerm * arithmeticTerm
      | Juxtaposition of arithmeticTerm * arithmeticTerm
      | Negation of arithmeticTerm
      | Mention of variation
      | Actualization of value
      | Aggregation of term
  and binding =
      Question of pattern * term
  and pattern =
      Element of symbol * pattern list
      | Variable of variation
      | Materialization of value
      | Procession of lyst
          (* For pattern-matching on reified terms *)
      | PtnSequence of var 
      | PtnApplication of var * var
      | PtnSupposition of var * var * var
      | PtnRecurrence of var * var * var
      | PtnAbstraction of var * var
      | PtnCondition of var * var * var
      | PtnComprehension of var * var
      | PtnConsolidation of var * var
      | PtnFiltration of var * var * var
      | PtnConcentration of var * var * var
      | PtnEquation of var * var
      | PtnComparisonLT of var * var
      | PtnComparisonGT of var * var
      | PtnComparisonLTE of var * var
      | PtnComparisonGTE of var * var
      | PtnReflection of var
      | PtnAcquisition
      | PtnSuspension of var * var
      | PtnRelease of var * var
      | PtnInnerSuspension of var * var
      | PtnDivision of var * var
      | PtnAddition of var * var
      | PtnMultiplication of var * var
      | PtnJuxtaposition of var * var
      | PtnNegation of var
  and variation =
      Identifer of var
      | Abandon of wild
  and lyst =
      Empty
      | Enum of pattern list
      | Cons of pattern list * lyst
      | ConsV of pattern list * variation
  and value =
      BooleanLiteral of duality
      | StringLiteral of string
      | IntegerLiteral of int
      | DoubleLiteral of float
      | Reification of term
  and duality =
      Verity
      | Absurdity
  and symbol =
      Tag of lIdent
  and lIdent = LIdent of string
  and uIdent = UIdent of string
  and wild = Wild of string    
end

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

exception NotYetImplemented of string

module rec NOMINAL :
  functor ( Term : TERMS ) ->
    sig
      type symbol
      type nominal =
          Transcription of Term.term
          | Symbol of symbol
      val comparator : nominal -> nominal -> bool
      val toString : nominal -> string
      val fresh : unit -> nominal
    end = functor ( Term : TERMS ) ->
struct
  type symbol
  type nominal =
      Transcription of Term.term
      | Symbol of symbol
  let comparator n1 n2 = raise (NotYetImplemented "comparator")
  let toString n = raise (NotYetImplemented "toString")
  let fresh () = raise (NotYetImplemented "fresh")
end
and TERM :
  functor ( Nominal : NOMINALS ) ->
sig
  type var = Nominal.nominal
  type term =
      Sequence of term list
      | Application of term * term list
      | Supposition of pattern * term * term
      | Recurrence of pattern * term * term
      | Abstraction of pattern * term
      | Condition of term * term * term
      | Comprehension of binding list * term
      | Consolidation of binding list * term
      | Filtration of binding list * pattern list * term
      | Concentration of binding list * pattern list * term
      | Equation of term * term
      | ComparisonLT of term * term
      | ComparisonGT of term * term
      | ComparisonLTE of term * term
      | ComparisonGTE of term * term
      | Reflection of var
      | Acquisition
      | Suspension of term * term
      | Release of term * term
      | InnerSuspension of term * term
      | Calculation of arithmeticTerm
  and arithmeticTerm =
      Division of arithmeticTerm * arithmeticTerm
      | Addition of arithmeticTerm * arithmeticTerm
      | Multiplication of arithmeticTerm * arithmeticTerm
      | Juxtaposition of arithmeticTerm * arithmeticTerm
      | Negation of arithmeticTerm
      | Mention of variation
      | Actualization of value
      | Aggregation of term
  and binding =
      Question of pattern * term
  and pattern =
      Element of symbol * pattern list
      | Variable of variation
      | Materialization of value
      | Procession of lyst
          (* For pattern-matching on reified terms *)
      | PtnSequence of var 
      | PtnApplication of var * var
      | PtnSupposition of var * var * var
      | PtnRecurrence of var * var * var
      | PtnAbstraction of var * var
      | PtnCondition of var * var * var
      | PtnComprehension of var * var
      | PtnConsolidation of var * var
      | PtnFiltration of var * var * var
      | PtnConcentration of var * var * var
      | PtnEquation of var * var
      | PtnComparisonLT of var * var
      | PtnComparisonGT of var * var
      | PtnComparisonLTE of var * var
      | PtnComparisonGTE of var * var
      | PtnReflection of var
      | PtnAcquisition
      | PtnSuspension of var * var
      | PtnRelease of var * var
      | PtnInnerSuspension of var * var
      | PtnDivision of var * var
      | PtnAddition of var * var
      | PtnMultiplication of var * var
      | PtnJuxtaposition of var * var
      | PtnNegation of var
  and variation =
      Identifer of var
      | Abandon of wild
  and lyst =
      Empty
      | Enum of pattern list
      | Cons of pattern list * lyst
      | ConsV of pattern list * variation
  and value =
      BooleanLiteral of duality
      | StringLiteral of string
      | IntegerLiteral of int
      | DoubleLiteral of float
      | Reification of term
  and duality =
      Verity
      | Absurdity
  and symbol =
      Tag of lIdent
  and lIdent = LIdent of string
  and uIdent = UIdent of string
  and wild = Wild of string    
end = functor ( Nominal : NOMINALS ) ->
struct
  type var = Nominal.nominal
  type term =
      Sequence of term list
      | Application of term * term list
      | Supposition of pattern * term * term
      | Recurrence of pattern * term * term
      | Abstraction of pattern * term
      | Condition of term * term * term
      | Comprehension of binding list * term
      | Consolidation of binding list * term
      | Filtration of binding list * pattern list * term
      | Concentration of binding list * pattern list * term
      | Equation of term * term
      | ComparisonLT of term * term
      | ComparisonGT of term * term
      | ComparisonLTE of term * term
      | ComparisonGTE of term * term
      | Reflection of var
      | Acquisition
      | Suspension of term * term
      | Release of term * term
      | InnerSuspension of term * term
      | Calculation of arithmeticTerm
  and arithmeticTerm =
      Division of arithmeticTerm * arithmeticTerm
      | Addition of arithmeticTerm * arithmeticTerm
      | Multiplication of arithmeticTerm * arithmeticTerm
      | Juxtaposition of arithmeticTerm * arithmeticTerm
      | Negation of arithmeticTerm
      | Mention of variation
      | Actualization of value
      | Aggregation of term
  and binding =
      Question of pattern * term
  and pattern =
      Element of symbol * pattern list
      | Variable of variation
      | Materialization of value
      | Procession of lyst
          (* For pattern-matching on reified terms *)
      | PtnSequence of var 
      | PtnApplication of var * var
      | PtnSupposition of var * var * var
      | PtnRecurrence of var * var * var
      | PtnAbstraction of var * var
      | PtnCondition of var * var * var
      | PtnComprehension of var * var
      | PtnConsolidation of var * var
      | PtnFiltration of var * var * var
      | PtnConcentration of var * var * var
      | PtnEquation of var * var
      | PtnComparisonLT of var * var
      | PtnComparisonGT of var * var
      | PtnComparisonLTE of var * var
      | PtnComparisonGTE of var * var
      | PtnReflection of var
      | PtnAcquisition
      | PtnSuspension of var * var
      | PtnRelease of var * var
      | PtnInnerSuspension of var * var
      | PtnDivision of var * var
      | PtnAddition of var * var
      | PtnMultiplication of var * var
      | PtnJuxtaposition of var * var
      | PtnNegation of var
  and variation =
      Identifer of var
      | Abandon of wild
  and lyst =
      Empty
      | Enum of pattern list
      | Cons of pattern list * lyst
      | ConsV of pattern list * variation
  and value =
      BooleanLiteral of duality
      | StringLiteral of string
      | IntegerLiteral of int
      | DoubleLiteral of float
      | Reification of term
  and duality =
      Verity
      | Absurdity
  and symbol =
      Tag of lIdent
  and lIdent = LIdent of string
  and uIdent = UIdent of string
  and wild = Wild of string
end

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
