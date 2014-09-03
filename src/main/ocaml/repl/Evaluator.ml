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
open Monad

(* The type of an abstract machine derived from a monadic evaluator *)
module type EVAL =
  functor ( M : MONAD ) ->
sig
  type 'a monad 
  type ident (* The type used for identifiers *)
  type term  (* The type used for terms in the language *)
  type arith_term (* The type used for arithmetic terms in the language *)
  type pattern (* The type used for patterns in the language *)
  type value (* The type of values *)
  type literal (* The type of literals *)
  type env (* The type of environments *)
  type ktn (* The type of continuations *)

  (* The reduction of terms and/or the transitions of the abstract machine *)
  val reduce : term -> env -> ktn -> value monad
  (* The primitive arithmetic operations *)
  val calculate : arith_term -> env -> ktn -> value monad
  (* Application of continuations *)
  val apply_k : ktn -> value -> value
  (* Application of closures *)
  val apply_closure : value -> value -> value

  (* Pattern-matching *)
  val unify : pattern -> value -> env option
  val materialize : literal -> value

  (* Divergence *)
  val bottom : value
  (* Unit *)
  val yunit : value
end 

(*
  An abstract machine derived from a monadic evaluator for a language that supports:
  reflection
  delimited continuations
  monadic comprehension
*)
module rec ReflectiveEval : EVAL =
  functor ( M : MONAD ) ->
struct
  type 'a monad = 'a M.monad
  type ident = ReflectiveNominal.nominal
  type term = ReflectiveTerm.term
  type arith_term = ReflectiveTerm.arithmeticTerm
  type pattern = ReflectiveTerm.pattern
  type literal = ReflectiveTerm.value
  type value = ReflectiveValue.value
  type env = ReflectiveValue.v_env
  type ktn = ( value, term ) ReflectiveK.cont

  exception NonFunctionInOpPosition of value 
  exception MatchFailure of pattern * value
  exception RuntimeException of string * term
      
  let bottom = ReflectiveValue.BOTTOM
  let yunit = ReflectiveValue.UNIT 

  let apply_closure op v =
    raise ( NotYetImplemented "apply_closure" )
  let materialize lit = 
    raise ( NotYetImplemented "materialize" )

  let rec reduce t e k =
    match t with 
        (* sequential composition *)
        ReflectiveTerm.Sequence( [] ) ->
          ( M.m_unit ( apply_k k yunit )  )
      | ReflectiveTerm.Sequence( thd :: [] ) ->
          ( reduce thd e k )
      | ReflectiveTerm.Sequence( thd :: ttl ) ->
          let _ = (reduce thd e k ) in 
          let rec loop ts =
            match ts with
                tshd :: [] -> (reduce tshd e k )
              | tshd :: tstl ->
                  let _ = (reduce tshd e k ) in
                    ( loop tstl )          
              | _ -> raise NotEnough
          in ( loop ttl )

      (* application *)
      | ReflectiveTerm.Application( op, [] ) ->
          ( M.m_bind
              ( reduce op e k )
              ( fun clsr ->
                match clsr with
                    ReflectiveValue.Closure( _, _, _ ) ->
                      ( M.m_unit ( apply_closure clsr ReflectiveTerm.UNIT ) )
                  | _ -> raise ( NonFunctionInOpPosition clsr )
              )
          )
      | ReflectiveTerm.Application( op, actls ) ->
          let bind_reduce acc actual =
            ( M.m_bind 
                ( reduce actual e k )
                ( fun a ->  
                  ( M.m_bind acc
                      ( fun clsr ->
                        match clsr with 
                            ReflectiveValue.Closure( _, _, _ ) ->
                              ( M.m_unit ( apply_closure clsr a ) )
                          | _ -> raise ( NonFunctionInOpPosition clsr )
                      )
                  )
                ) 
            ) in
            ( List.fold_left bind_reduce ( reduce op e k ) actls )

      (* let *)
      | ReflectiveTerm.Supposition( ptn, pterm, eterm ) ->
          ( M.m_bind
              ( reduce pterm e k )
              ( fun a ->                
                match ( ( unify ptn a ), e ) with
                    ( Some( ptn_env ), ReflectiveValue.Env( renv ) ) ->
                      ( reduce
                          eterm
                          ( ReflectiveValue.Env( ReflectiveEnv.sum ptn_env renv ) )
                          k )
                  | _ -> raise ( MatchFailure ( ptn, a ) )
              )
          )

      (* letrec *)
      | ReflectiveTerm.Recurrence( ptn, pterm, eterm ) ->
          raise ( NotYetImplemented "Recurrence" )

      (* abstraction *)
      | ReflectiveTerm.Abstraction( ptn, eterm ) ->
          ( M.m_unit ( apply_k k ( ReflectiveValue.Closure( ptn, eterm, e ) ) ) )
            
      (* condition *)            
      | ReflectiveTerm.Condition( test, tbranch, fbranch ) ->
          ( M.m_bind
              ( reduce test e k )
              ( fun a ->
                ( match a with
                    ReflectiveValue.Ground( ReflectiveValue.Boolean( true ) ) -> ( reduce tbranch e k )
                  | ReflectiveValue.Ground( ReflectiveValue.Boolean( false ) ) -> ( reduce fbranch e k )
                  | _ -> raise ( RuntimeException ( "expected Boolean", test ) ) ) ) )

      (* monadic desugaring *)
      | ReflectiveTerm.Comprehension( bindings, eterm ) -> 
          raise ( NotYetImplemented "Comprehension" )
      | ReflectiveTerm.Consolidation( bindings, eterm ) ->
          raise ( NotYetImplemented "Consolidation" )
      | ReflectiveTerm.Filtration( bindings, conditions, eterm ) ->
          raise ( NotYetImplemented "Filtration" )
      | ReflectiveTerm.Concentration( bindings, conditions, eterm ) ->
          raise ( NotYetImplemented "Concentration" )

      (* comparison *)
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

      (* reflection -- dual to reification *)
      | ReflectiveTerm.Reflection( v ) ->
          raise ( NotYetImplemented "Reflection" )

      (* delimited continuations *)
      | ReflectiveTerm.Acquisition -> 
          raise ( NotYetImplemented "Acquisition" )
      | ReflectiveTerm.Suspension( pterm, eterm ) -> 
          raise ( NotYetImplemented "Suspension" )
      | ReflectiveTerm.Release( pterm, eterm ) -> 
          raise ( NotYetImplemented "Release" )
      | ReflectiveTerm.InnerSuspension( pterm, eterm ) -> 
          raise ( NotYetImplemented "InnerSuspension" )

      (* primitive arithmetic calculation *)
      | ReflectiveTerm.Calculation( aterm ) -> 
          ( calculate aterm e k )
  and calculate a e k =
    match a with 
        Division( aterm1, aterm2 ) ->
          ( M.m_bind
              ( calculate aterm1 e k )
              ( fun a ->
                ( M.m_bind
                    ( calculate aterm2 e k )
                    ( fun b ->
                      match ( a, b ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Double ( d1 /. d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Double ( ( float d1 ) /. d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Double ( d1 /. float( d2 ) ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Integer ( d1 / d2 ) ) ) ) )
                    )
                )
              )
          )
            
      | Addition( aterm1, aterm2 ) ->
          ( M.m_bind
              ( calculate aterm1 e k )
              ( fun a ->
                ( M.m_bind
                    ( calculate aterm2 e k )
                    ( fun b ->
                      match ( a, b ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Double ( d1 +. d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Double ( ( float d1 ) +. d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Double ( d1 +. float( d2 ) ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Integer ( d1 + d2 ) ) ) ) )
                    )
                )
              )
          )
      | Multiplication( aterm1, aterm2 ) ->
          ( M.m_bind
              ( calculate aterm1 e k )
              ( fun a ->
                ( M.m_bind
                    ( calculate aterm2 e k )
                    ( fun b ->
                      match ( a, b ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Double ( d1 *. d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Double ( ( float d1 ) *. d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Double ( d1 *. float( d2 ) ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Integer ( d1 * d2 ) ) ) ) )
                    )
                )
              )
          )
      | Juxtaposition( aterm1, aterm2 ) ->
          raise ( NotYetImplemented "Juxtaposition" )
      | Negation( aterm ) ->
          raise ( NotYetImplemented "Negation" )
      | Mention( n ) ->
          ( match ( n, e ) with
              ( Identifier( v ), ReflectiveValue.Env( renv ) ) ->
(*                 ( M.m_unit ( apply_k k ( ReflectiveEnv.lookup ( v, renv ) ) ) ) *)
                raise ( NotYetImplemented "Mention lookup" )
            | _ -> raise ( NotYetImplemented "Mention wildcard" ) )
      | Actualization( aterm ) ->
          raise ( NotYetImplemented "Actualization" )
      | Aggregation( aterm ) ->
          raise ( NotYetImplemented "Aggregation" )
  and apply_k k v =
    raise ( NotYetImplemented "apply_k" )  
  and unify p t =
    let rslt : env = ( ReflectiveValue.Env ReflectiveEnv.empty ) in
      ( Some rslt )
    (* match ( p, t ) with *)
(*         ( ReflectiveTerm.Element( fnctr, sptns ), t ) -> *)
(*           raise ( NotYetImplemented "unify Element" ) *)
(*       | ( ReflectiveTerm.Variable( ReflectiveTerm.Identifier( n ) ), t ) -> *)
(*           let nmap : ( ident, value ) ReflectiveEnv.map = ( ReflectiveEnv.extend ( n, t, ( ReflectiveEnv.empty ) ) ) in *)
(*           let vnenv : env = ( ReflectiveValue.Env nmap ) in *)
(*             ( Some vnenv )  *)
(*       | ( ReflectiveTerm.Materialization( c ), t ) ->  *)
(*           let m = ( materialize c ) in  *)
(*             ( match ( m == t ) with   *)
(*                 true -> ( Some ( ReflectiveEnv.empty ) )   *)
(*               | false -> raise ( MatchFailure ( p, t ) ) )  *)
(*       | ( ReflectiveTerm.Procession( l ), t ) -> *)
(*           raise ( NotYetImplemented "unify Procession" ) *)
(*       | ( ReflectiveTerm.PtnSequence( v ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnSequence" ) *)
(*       | ( ReflectiveTerm.PtnApplication( fn, actls ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnApplication" ) *)
(*       | ( ReflectiveTerm.PtnSupposition( lptn, ltrm, lbody ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnSupposition" ) *)
(*       | ( ReflectiveTerm.PtnRecurrence( lptn, ltrm, lbody ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnRecurrence" ) *)
(*       | ( ReflectiveTerm.PtnAbstraction( ptn, body ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnRecurrence" ) *)
(*       | ( ReflectiveTerm.PtnCondition( ptest, ptrue, pfalse ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnCondition" ) *)
(*       | ( ReflectiveTerm.PtnComprehension( pbnds, pbdy ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnComprehension" ) *)
(*       | ( ReflectiveTerm.PtnConsolidation( pbnds, pbdy ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnConsolidation" ) *)
(*       | ( ReflectiveTerm.PtnFiltration( pbnds, pbdy, pcnds ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnConsolidation" ) *)
(*       | ( ReflectiveTerm.PtnConcentration( pbnds, pbdy, pcnds ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnConcentration" ) *)
(*       | ( ReflectiveTerm.PtnEquation( pl, pr ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnEquation" ) *)
(*       | ( ReflectiveTerm.PtnComparisonLT( pl, pr ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnComparisonLT" ) *)
(*       | ( ReflectiveTerm.PtnComparisonGT( pl, pr ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnComparisonGT" ) *)
(*       | ( ReflectiveTerm.PtnComparisonLTE( pl, pr ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnComparisonLTE" ) *)
(*       | ( ReflectiveTerm.PtnComparisonGTE( pl, pr ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnComparisonGTE" ) *)
(*       | ( ReflectiveTerm.PtnReflection( v ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnReflection" ) *)
(*       | ( ReflectiveTerm.PtnAcquisition, t ) -> *)
(*           raise ( NotYetImplemented "unify PtnAcquisition" ) *)
(*       | ( ReflectiveTerm.PtnSuspension( u, v ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnSuspension" ) *)
(*       | ( ReflectiveTerm.PtnRelease( u, v ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnRelease" ) *)
(*       | ( ReflectiveTerm.PtnInnerSuspension( u, v ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnInnerSuspension" ) *)
(*       | ( ReflectiveTerm.PtnDivision( v1, v2 ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnDivision" ) *)
(*       | ( ReflectiveTerm.PtnAddition( v1, v2 ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnAddition" ) *)
(*       | ( ReflectiveTerm.PtnMultiplication( v1, v2 ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnMultiplication" ) *)
(*       | ( ReflectiveTerm.PtnJuxtaposition( v1, v2 ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnJuxtaposition" ) *)
(*       | ( ReflectiveTerm.PtnNegation( n ), t ) -> *)
(*           raise ( NotYetImplemented "unify PtnNegation" )   *)
  
end
(* This gives a simple and effective form of reflection for quasiquote *)
and ReflectiveNominal : NOMINALS = NOMINAL( ReflectiveTerm )
and ReflectiveTerm : TERMS with type var = ReflectiveNominal.nominal = TERM( ReflectiveNominal ) 
and ReflectiveValue : VALUES with type ident = ReflectiveNominal.nominal
                             and type term = ReflectiveTerm.term
                             and type pattern = ReflectiveTerm.pattern
                             and type ('n, 'v) environment = ('n, 'v) ReflectiveEnv.map
  = VALUEFUNCTOR( ReflectiveNominal )( ReflectiveTerm )( ReflectiveEnv )
and ReflectiveK : CONTINUATIONS with type nominal = ReflectiveNominal.nominal
                                and type ('n, 'v) k_env = ('n, 'v) ReflectiveEnv.map
  = CONTINUATIONFUNCTOR( ReflectiveNominal )( ReflectiveEnv )
and ReflectiveEnv : ENVIRONMENTS = ListEnv (* ReflectiveNominal *)
