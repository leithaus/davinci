(* -*- mode: Tuareg;-*-  *)
(* Filename:    Knot.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Fri Aug 22 00:02:12 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Nominals
open Terms
open Exceptions

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

