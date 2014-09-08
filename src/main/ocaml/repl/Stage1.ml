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
open Symbols
open Evaluator

module type ASTXFORMS =
sig
  module REval : EVAL

  type model_ident = REval.ident
  type model_term = REval.term
  type model_arith_term = REval.arith_term
  type model_pattern = REval.pattern
  type model_binding = REval.binding
  type model_literal = REval.literal
  type model_value = REval.value
  type model_lyst = REval.lyst

  val expr_to_term : expr -> model_term
  val ptrn_to_pattern : pattern -> model_pattern
  val bind_to_binding : binding -> model_binding
  val lptrn_to_lpattern : lyst -> model_lyst
  val literal_to_value : value -> model_literal
  val var_to_ident : variation -> model_ident
  val calc_to_calculation : arithmeticExpr -> model_arith_term 
end

module type ASTXFORMFUNCTOR =
  functor ( M : MONAD ) ->
sig
  module REval : ( EVAL with type 'a monad = 'a M.monad )

  type model_ident = REval.ident
  type model_term = REval.term
  type model_arith_term = REval.arith_term
  type model_pattern = REval.pattern
  type model_binding = REval.binding
  type model_literal = REval.literal
  type model_value = REval.value
  type model_lyst = REval.lyst

  val expr_to_term : expr -> model_term
  val ptrn_to_pattern : pattern -> model_pattern
  val bind_to_binding : binding -> model_binding
  val lptrn_to_lpattern : lyst -> model_lyst
  val literal_to_value : value -> model_literal
  val var_to_ident : variation -> model_ident
  val calc_to_calculation : arithmeticExpr -> model_arith_term 
end

module ASTXFORM : ASTXFORMFUNCTOR =
  functor ( M : MONAD ) ->
struct
  module REval : ( EVAL with type 'a monad = 'a M.monad )
    = ReflectiveEval( M )

  type model_ident = REval.ident
  type model_term = REval.term
  type model_arith_term = REval.arith_term
  type model_pattern = REval.pattern
  type model_binding = REval.binding
  type model_literal = REval.literal
  type model_value = REval.value
  type model_lyst = REval.lyst

  let rec expr_to_term e =
    ( match e with
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
            ( List.map ( fun actl -> ( expr_to_term actl ) ) actls )
          in 
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
          ( REval.ReflectiveTerm.Calculation
              ( calc_to_calculation a_expr ) ) )
    and calc_to_calculation a_expr =
        match a_expr with
            Division( a1_expr, a2_expr ) ->
              ( REval.ReflectiveTerm.Division
                  (
                    ( calc_to_calculation a1_expr ),
                    ( calc_to_calculation a2_expr )
                  )
              )                  
          | Addition( a1_expr, a2_expr ) ->
              ( REval.ReflectiveTerm.Addition
                  (
                    ( calc_to_calculation a1_expr ),
                    ( calc_to_calculation a2_expr )
                  )
              )
          | Multiplication( a1_expr, a2_expr ) ->
              ( REval.ReflectiveTerm.Multiplication
                  (
                    ( calc_to_calculation a1_expr ),
                    ( calc_to_calculation a2_expr )
                  )
              )
          | Juxtaposition( a1_expr, a2_expr ) ->
              ( REval.ReflectiveTerm.Juxtaposition
                  (
                    ( calc_to_calculation a1_expr ),
                    ( calc_to_calculation a2_expr )
                  )
              )
          | Negation( a_expr ) ->
              ( REval.ReflectiveTerm.Negation
                  ( calc_to_calculation a_expr )
              )
          | Mention( v ) ->
              ( REval.ReflectiveTerm.Mention
                  ( REval.ReflectiveTerm.Identifier
                      ( var_to_ident v ) )
              )
          | Actualization( lit ) ->
              ( REval.ReflectiveTerm.Actualization
                  ( literal_to_value lit ) )
          | Aggregation( expr ) ->
              ( REval.ReflectiveTerm.Aggregation ( expr_to_term expr ) )
  and ptrn_to_pattern ptn = 
    ( match ptn with 
        Element( Tag( LIdent( fnctr ) ), elems_expr ) -> 
          (
            let fnctr_term = 
              ( REval.ReflectiveTerm.Tag
                  ( REval.ReflectiveTerm.LIdent fnctr ) ) in 
            let elems_term =
              ( List.map ptrn_to_pattern elems_expr ) in 
              ( REval.ReflectiveTerm.Element( fnctr_term, elems_term ) )
          ) 
      | Variable( Atomic( UIdent( v ) ) ) -> 
          ( REval.ReflectiveTerm.Variable
              ( REval.ReflectiveTerm.Identifier
                  ( REval.ReflectiveNominal.Symbol ( Symbols.Opaque v ) ) )
          )
      | Variable( Abandon( Wild( s ) ) ) ->
          (
            REval.ReflectiveTerm.Variable 
              ( REval.ReflectiveTerm.Abandon
                  ( REval.ReflectiveTerm.Wild s ) ) 
          )
      | Variable( Transcription( expr ) ) ->
          (
            REval.ReflectiveTerm.Variable
              ( REval.ReflectiveTerm.Identifier
                  ( REval.ReflectiveNominal.Transcription
                      ( expr_to_term expr ) ) )
          )
      | Materialization( literal ) ->
          ( REval.ReflectiveTerm.Materialization ( literal_to_value literal ) )
      | Procession( lst ) ->
          ( REval.ReflectiveTerm.Procession ( lptrn_to_lpattern lst ) )
      | PtnSequence( v ) ->
          ( REval.ReflectiveTerm.PtnSequence
              ( var_to_ident v ) )
      | PtnApplication( v_op, v_args ) ->
          ( REval.ReflectiveTerm.PtnApplication
              (
                ( var_to_ident v_op ),
                ( var_to_ident v_args )
              )
          )
      | PtnSupposition( v_ptrn, v_p_expr, v_t_expr ) ->
          ( REval.ReflectiveTerm.PtnSupposition
              (
                ( var_to_ident v_ptrn ),
                ( var_to_ident v_p_expr ),
                ( var_to_ident v_t_expr )
              )
          )
      | PtnRecurrence( v_ptrn, v_p_expr, v_t_expr ) ->
          ( REval.ReflectiveTerm.PtnRecurrence
              (
                ( var_to_ident v_ptrn ),
                ( var_to_ident v_p_expr ),
                ( var_to_ident v_t_expr )
              )
          )
      | PtnAbstraction( v_ptrn, v_t_expr ) ->
          ( REval.ReflectiveTerm.PtnAbstraction
              (
                ( var_to_ident v_ptrn ),
                ( var_to_ident v_t_expr )
              )
          )
      | PtnCondition( v_test, v_t_expr, v_f_expr ) ->
          ( REval.ReflectiveTerm.PtnCondition
              (
                ( var_to_ident v_test ),
                ( var_to_ident v_t_expr ),
                ( var_to_ident v_f_expr )
              )
          )
      | PtnComprehend( v_binds, v_t_expr ) ->
          ( REval.ReflectiveTerm.PtnComprehension
              (
                ( var_to_ident v_binds ),
                ( var_to_ident v_t_expr )
              )
          )
      | PtnConsolidate( v_binds, v_t_expr ) ->
          ( REval.ReflectiveTerm.PtnConsolidation
              (
                ( var_to_ident v_binds ),
                ( var_to_ident v_t_expr )
              )
          )
      | PtnFiltration( v_binds, v_conds, v_t_expr ) ->
          ( REval.ReflectiveTerm.PtnFiltration
              (
                ( var_to_ident v_binds ),
                ( var_to_ident v_conds ),
                ( var_to_ident v_t_expr )
              )
          )
      | PtnConcentrate( v_binds, v_conds, v_t_expr ) ->
          ( REval.ReflectiveTerm.PtnConcentration
              (
                ( var_to_ident v_binds ),
                ( var_to_ident v_conds ),
                ( var_to_ident v_t_expr )
              )
          )
      | PtnEquation( v_l_expr, v_r_expr ) ->
          ( REval.ReflectiveTerm.PtnEquation
              (
                ( var_to_ident v_l_expr ),
                ( var_to_ident v_r_expr )
              )
          )
      | PtnCompLT( v_l_expr, v_r_expr ) ->
          ( REval.ReflectiveTerm.PtnComparisonLT
              (
                ( var_to_ident v_l_expr ),
                ( var_to_ident v_r_expr )
              )
          )
      | PtnCompGT( v_l_expr, v_r_expr ) ->
          ( REval.ReflectiveTerm.PtnComparisonGT
              (
                ( var_to_ident v_l_expr ),
                ( var_to_ident v_r_expr )
              )
          )
      | PtnCompLTE( v_l_expr, v_r_expr ) ->
          ( REval.ReflectiveTerm.PtnComparisonLTE
              (
                ( var_to_ident v_l_expr ),
                ( var_to_ident v_r_expr )
              )
          )
      | PtnCompGTE( v_l_expr, v_r_expr ) ->
          ( REval.ReflectiveTerm.PtnComparisonGTE
              (
                ( var_to_ident v_l_expr ),
                ( var_to_ident v_r_expr )
              )
          )
      | PtnReflection( v1, v2 ) ->
          (* BUGBUG -- lgm -- there's a bug in the grammar! *)
          ( REval.ReflectiveTerm.PtnReflection
              ( var_to_ident v1 ) )  
      | PtnAcquisition ->
          REval.ReflectiveTerm.PtnAcquisition
      | PtnSuspension( v_prompt, v_t_expr ) ->
          ( REval.ReflectiveTerm.PtnSuspension
              (
                ( var_to_ident v_prompt ),
                ( var_to_ident v_t_expr )
              )
          )
      | PtnRelease( v_prompt, v_t_expr ) ->
          ( REval.ReflectiveTerm.PtnRelease
              (
                ( var_to_ident v_prompt ),              
                ( var_to_ident v_t_expr )
              )
          )
      | PtnInnerSuspend( v_prompt, v_t_expr ) ->
          ( REval.ReflectiveTerm.PtnInnerSuspension
              (
                ( var_to_ident v_prompt ),
                ( var_to_ident v_t_expr )
              )
          )
      | PtnDivision( v_l_expr, v_r_expr ) ->
          ( REval.ReflectiveTerm.PtnDivision
              (
                ( var_to_ident v_l_expr ),
                ( var_to_ident v_r_expr )
              )
          )
      | PtnAddition( v_l_expr, v_r_expr ) ->
          ( REval.ReflectiveTerm.PtnAddition
              (
                ( var_to_ident v_l_expr ),
                ( var_to_ident v_r_expr )
              )
          )
      | PtnMultiply( v_l_expr, v_r_expr ) ->
          ( REval.ReflectiveTerm.PtnMultiplication
              (
                ( var_to_ident v_l_expr ),
                ( var_to_ident v_r_expr )
              )
          )
      | PtnJuxtapose( v_l_expr, v_r_expr ) ->
          ( REval.ReflectiveTerm.PtnJuxtaposition
              (
                ( var_to_ident v_l_expr ),
                ( var_to_ident v_r_expr )
              )
          )
      | PtnNegate( v1, v2 ) ->
          (* BUGBUG -- lgm -- there's a bug in the grammar! *)
          ( REval.ReflectiveTerm.PtnNegation
              ( var_to_ident v1 ) ) )
  and lptrn_to_lpattern lst_ptn =
    ( match lst_ptn with 
        Empty -> REval.ReflectiveTerm.Empty 
      | Enum( ptn_expr_lst ) -> 
          ( REval.ReflectiveTerm.Enum ( List.map ptrn_to_pattern ptn_expr_lst ) )
      | Cons( ptn_expr_lst, rlst_ptn ) -> 
          ( REval.ReflectiveTerm.Cons
              ( 
                ( List.map ptrn_to_pattern ptn_expr_lst ),
                ( lptrn_to_lpattern rlst_ptn ) 
              )
          ) 
      | ConsV( ptn_expr_lst, rlst_v ) ->
          ( REval.ReflectiveTerm.ConsV
              ( 
                ( List.map ptrn_to_pattern ptn_expr_lst ),
                ( REval.ReflectiveTerm.Identifier ( var_to_ident rlst_v ) )
              ) 
          )
    )
  and bind_to_binding b = 
    match b with 
        Question( ptn_expr, t_expr ) ->  
          (
            REval.ReflectiveTerm.Question  
              (
                ( ptrn_to_pattern ptn_expr ),
                ( expr_to_term t_expr )
              )
          )
  and literal_to_value literal =
    ( match literal with  
        BooleanLiteral( Verity ) ->  
          ( REval.ReflectiveTerm.BooleanLiteral 
              REval.ReflectiveTerm.Verity ) 
      | BooleanLiteral( Absurdity ) -> 
          ( REval.ReflectiveTerm.BooleanLiteral 
              REval.ReflectiveTerm.Absurdity )
      | StringLiteral( s ) -> 
          ( REval.ReflectiveTerm.StringLiteral s ) 
      | IntegerLiteral( i ) -> 
          ( REval.ReflectiveTerm.IntegerLiteral i ) 
      | DoubleLiteral( d ) -> 
          ( REval.ReflectiveTerm.DoubleLiteral d ) 
      | Reification( e ) -> 
          ( REval.ReflectiveTerm.Reification ( expr_to_term e ) ) )     
  and var_to_ident v =
    match v with
        Atomic( UIdent( s ) ) ->
          ( REval.ReflectiveNominal.Symbol ( Symbols.Opaque s ) )
            (* BUGBUG -- lgm -- this doesn't line up with term Wild *)
      | Abandon( Wild( s ) ) -> 
          ( REval.ReflectiveNominal.Symbol ( Symbols.Opaque s ) )
      | Transcription( expr ) ->
          ( REval.ReflectiveNominal.Transcription
              ( expr_to_term expr ) )
end
