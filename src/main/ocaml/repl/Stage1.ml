(* -*- mode: Tuareg;-*-  *)
(* Filename:    Stage1.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Thu Sep  4 00:52:07 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Abscacao
open Exceptions
open Monad
open Evaluator

module type ASTXFORMS =
sig
  module REval : EVAL

  type model_term = REval.term
  type model_pattern = REval.pattern
  type model_binding = REval.binding

  val expr_to_term : expr -> model_term
  val ptrn_to_pattern : pattern -> model_pattern
  val bind_to_binding : binding -> model_binding
end

module type ASTXFORMFUNCTOR =
  functor ( M : MONAD ) ->
sig
  module REval : EVAL

  type model_term = REval.term
  type model_pattern = REval.pattern
  type model_binding = REval.binding

  val expr_to_term : expr -> model_term
  val ptrn_to_pattern : pattern -> model_pattern
  val bind_to_binding : binding -> model_binding
end

module ASTXFORM : ASTXFORMFUNCTOR =
  functor ( M : MONAD ) ->
struct
  module REval : EVAL = ReflectiveEval( M )

  type model_term = REval.term
  type model_pattern = REval.pattern
  type model_binding = REval.binding

  let rec expr_to_term e =
    match e with
        Sequence( e1, e2 ) ->
          (
            let t1 = ( expr_to_term e1 ) in
            let t2 = ( expr_to_term e2 ) in
              ( REval.ReflectiveTerm.Sequence [ t1; t2 ] )
          )
      | Application( op, actls ) ->
          let t_op =
            ( expr_to_term op ) in
          let t_actls =
            ( List.map ( fun actl -> ( expr_to_term actl ) ) actls ) in
            ( REval.ReflectiveTerm.Application ( t_op, t_actls ) )
      | Supposition( ptn, p_expr, e_expr ) ->
          let t_ptn = ( ptrn_to_pattern ptn ) in
          let p_term = ( expr_to_term p_expr ) in
          let e_term = ( expr_to_term e_expr ) in
            ( REval.ReflectiveTerm.Supposition ( t_ptn, p_term, e_term ) )
      | Recurrence( ptn, p_expr, e_expr ) ->
          let t_ptn = ( ptrn_to_pattern ptn ) in
          let p_term = ( expr_to_term p_expr ) in
          let e_term = ( expr_to_term e_expr ) in
            ( REval.ReflectiveTerm.Recurrence ( t_ptn, p_term, e_term ) )
      | Abstraction( ptn, e_expr ) ->
          let t_ptn = ( ptrn_to_pattern ptn ) in
          let e_term = ( expr_to_term e_expr ) in
            ( REval.ReflectiveTerm.Abstraction ( t_ptn, e_term ) )
      | Condition( test_expr, t_expr, f_expr ) ->
          let test_term = ( expr_to_term test_expr ) in
          let t_term = ( expr_to_term t_expr ) in
          let f_term = ( expr_to_term f_expr ) in
            ( REval.ReflectiveTerm.Condition ( test_term, t_term, f_term ) )
      | Comprehension( binds, e_expr ) ->
          let bindings =
            ( List.map ( fun b -> ( bind_to_binding b ) ) binds ) in
          let e_term = ( expr_to_term e_expr ) in
            ( REval.ReflectiveTerm.Comprehension ( bindings, e_term ) )
      | Consolidation( binds, e_expr ) ->
          let bindings =
            ( List.map ( fun b -> ( bind_to_binding b ) ) binds ) in
          let e_term = ( expr_to_term e_expr ) in
            ( REval.ReflectiveTerm.Consolidation ( bindings, e_term ) )
      | Filtration( binds, ptns, e_expr ) ->
          let bindings =
            ( List.map ( fun b -> ( bind_to_binding b ) ) binds ) in
          let patterns =
            ( List.map ( fun ptn -> ( ptrn_to_pattern ptn ) ) ptns ) in
          let e_term = ( expr_to_term e_expr ) in
            ( REval.ReflectiveTerm.Filtration ( bindings, patterns, e_term ) )
      | Concentration( binds, ptns, e_expr ) ->
          let bindings =
            ( List.map ( fun b -> ( bind_to_binding b ) ) binds ) in
          let patterns =
            ( List.map ( fun ptn -> ( ptrn_to_pattern ptn ) ) ptns ) in
          let e_term = ( expr_to_term e_expr ) in
            ( REval.ReflectiveTerm.Concentration ( bindings, patterns, e_term ) )
      | Equation( l_expr, r_expr ) ->
          let l_term = ( expr_to_term l_expr ) in
          let r_term = ( expr_to_term r_expr ) in
            ( REval.ReflectiveTerm.Equation ( l_term, r_term ) )
      | ComparisonLT( l_expr, r_expr ) ->
          let l_term = ( expr_to_term l_expr ) in
          let r_term = ( expr_to_term r_expr ) in
            ( REval.ReflectiveTerm.ComparisonLT ( l_term, r_term ) )
      | ComparisonGT( l_expr, r_expr ) ->
          let l_term = ( expr_to_term l_expr ) in
          let r_term = ( expr_to_term r_expr ) in
            ( REval.ReflectiveTerm.ComparisonGT ( l_term, r_term ) )
      | ComparisonLTE( l_expr, r_expr ) ->
          let l_term = ( expr_to_term l_expr ) in
          let r_term = ( expr_to_term r_expr ) in
            ( REval.ReflectiveTerm.ComparisonLTE ( l_term, r_term ) )
      | ComparisonGTE( l_expr, r_expr ) ->
          let l_term = ( expr_to_term l_expr ) in
          let r_term = ( expr_to_term r_expr ) in
            ( REval.ReflectiveTerm.ComparisonGTE ( l_term, r_term ) )
      | Reflection( v ) ->
          raise ( NotYetImplemented "AST xform: Reflection" )
      | Acquisition ->
          raise ( NotYetImplemented "AST xform: Acquisition" )
      | Suspension( l_expr, r_expr ) ->
          let l_term = ( expr_to_term l_expr ) in
          let r_term = ( expr_to_term r_expr ) in
            ( REval.ReflectiveTerm.Suspension ( l_term, r_term ) )
      | Release( l_expr, r_expr ) ->
          let l_term = ( expr_to_term l_expr ) in
          let r_term = ( expr_to_term r_expr ) in
            ( REval.ReflectiveTerm.Release ( l_term, r_term ) )
      | InnerSuspension( l_expr, r_expr ) ->
          let l_term = ( expr_to_term l_expr ) in
          let r_term = ( expr_to_term r_expr ) in
            ( REval.ReflectiveTerm.InnerSuspension ( l_term, r_term ) )
      | Calculation( a_expr ) ->
          raise ( NotYetImplemented "AST xform: Calculation" )
  and ptrn_to_pattern ptn =
    raise ( NotYetImplemented "ptrn_to_pattern" )
  and bind_to_binding bind =
    raise ( NotYetImplemented "bind_to_binding" )
end
