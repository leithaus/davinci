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

module type EVAL =
  functor ( M : MONAD ) ->
sig
  type 'a monad 
  type ident
  type term
  type pattern
  type value
  type env
  type ktn
      
  val reduce : term -> env -> ktn -> value monad
  val apply_closure : value -> value -> value
  val apply_k : ktn -> value -> value

  val unify : pattern -> value -> env option

  val bottom : value
  val yunit : value
end 

module rec ReflectiveEval : EVAL =
  functor ( M : MONAD ) ->
struct
  type 'a monad = 'a M.monad
  type ident = ReflectiveNominal.nominal
  type term = ReflectiveTerm.term
  type pattern = ReflectiveTerm.pattern
  type value = ReflectiveValue.value
  type env = ReflectiveValue.env
  type ktn = ( value, term ) ReflectiveK.cont

  exception NonFunctionInOpPosition of value 
  exception MatchFailure of pattern * term
  exception RuntimeException of string * term
      
  let bottom = ReflectiveValue.BOTTOM
  let yunit = ReflectiveValue.UNIT 

  let unify p t =
    raise ( NotYetImplemented "unify" ) 

  let apply_closure op v =
    raise ( NotYetImplemented "apply_closure" ) 

  let rec reduce t e k =
    match t with 
        (* sequential composition *)
        ReflectiveTerm.Sequence( [] ) ->
          ( M.m_unit yunit ) 
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
                  | _ -> raise ( MatchFailure ( ptn, pterm ) )
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
          raise ( NotYetImplemented "Calculuation" )   
  and apply_k k v =
    raise ( NotYetImplemented "apply_k" )
  
end
(* This gives a simple and effective form of reflection for quasiquote *)
and ReflectiveNominal : NOMINALS = NOMINAL( ReflectiveTerm )
and ReflectiveTerm : TERMS = TERM( ReflectiveNominal ) 
and ReflectiveValue : VALUES with type ident = ReflectiveNominal.nominal
                             and type term = ReflectiveTerm.term
                             and type pattern = ReflectiveTerm.pattern
                             and type ('n, 'v) environment = ('n, 'v) ReflectiveEnv.env
  = VALUEFUNCTOR( ReflectiveNominal )( ReflectiveTerm )( ReflectiveEnv )
and ReflectiveK : CONTINUATIONS = CONTINUATIONFUNCTOR( ReflectiveNominal )( ReflectiveEnv )
and ReflectiveEnv : ENVIRONMENTS = ListEnv (* ReflectiveNominal *)
