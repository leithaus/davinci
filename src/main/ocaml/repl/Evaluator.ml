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
open Symbols

(*
  The type of a monadic evaluator dependent on a monad and a collection of
  algebraic theories for

  - names/variables/identifiers
  - terms
  - values
  - continuations
  - environments

  Note that there is a spectrum of evaluation methods from monadic evaluator to 
  abstract machine. Thus, the function reduce may polymorphically represent the reduction
  arrow of an operational semantics, or the transitions of an abstract machine.

  This correspondence, far from being a nicety, is essential for practical proofs
  of the correctness of execution.

  Of special interest is built in support for 
  
  - reflection 
  - delimited continuations
  - monadic comprehensions
*)
module type EVAL = 
sig
  (* Algebraic theory of names, variables, identifiers *)
  module rec ReflectiveNominal : ( NOMINALS with type symbol = Symbols.symbol
                                            and type term = ReflectiveTerm.term )
    (* Algebraic theory of terms *)
  and ReflectiveTerm : ( TERMS with type var = ReflectiveNominal.nominal )     
    (* Algebraic theory of values *)
  and ReflectiveValue : ( VALUES with type ident = ReflectiveNominal.nominal
                                 and type term = ReflectiveTerm.term
                                 and type pattern = ReflectiveTerm.pattern
                                 and type ('n, 'v) environment = ('n, 'v) ReflectiveEnv.map )    
    (* Algebraic theory of continuations *)
  and ReflectiveK : ( CONTINUATIONS with type nominal = ReflectiveNominal.nominal
                                    and type ('n, 'v) k_env = ('n, 'v) ReflectiveEnv.map )
    (* Algebraic theory of environments *)
  and ReflectiveEnv : ENVIRONMENTS

  type 'a monad 

      (* The types in the algebraic theories we use in the semantic model *) 

      (* The type used for identifiers *) 
  type ident = ReflectiveNominal.nominal
      (* The type used for terms in the language *)
  type term = ReflectiveTerm.term
      (* The type used for arithmetic terms in the language *)
  type arith_term = ReflectiveTerm.arithmeticTerm
      (* The type used for patterns in the language *)
  type pattern = ReflectiveTerm.pattern
      (* The type used for list patterns in the language *)
  type lyst = ReflectiveTerm.lyst
      (* The type used for binding declarations in the language *)
  type binding = ReflectiveTerm.binding
      (* The type of literals *)
  type literal = ReflectiveTerm.value
      (* The type of values *)
  type value = ReflectiveValue.value
      (* The type of environments *)
  type env = ReflectiveValue.v_env
      (* The type of continuations *)
  type ktn = ( value, term ) ReflectiveK.cont

  (* The reduction of terms and/or the transitions of the abstract machine *)
  val reduce : term -> env -> ktn -> value monad
  (* The primitive arithmetic operations *)
  val calculate : arith_term -> env -> ktn -> value monad
  (* Application of continuations *)
  val apply_k : ktn -> value -> value
  (* Application of closures *)
  val apply_closure : value -> value -> ktn -> value monad

  (* Pattern-matching *)
  val unify : pattern -> value -> env option
  val materialize : literal -> value

  (* Divergence *)
  val bottom : value
  (* Unit *)
  val yunit : value
    
  val init_env : env
  val init_k : ktn
end 

(* The type of an abstract machine derived from a monadic evaluator *)
module type EVALFUNCTOR =
  functor ( M : MONAD ) ->
sig
  (* Algebraic theory of names, variables, identifiers *)
  module rec ReflectiveNominal : ( NOMINALS with type symbol = Symbols.symbol
                                            and type term = ReflectiveTerm.term )
    (* Algebraic theory of terms *)
  and ReflectiveTerm : ( TERMS with type var = ReflectiveNominal.nominal)     
    (* Algebraic theory of values *)
  and ReflectiveValue : ( VALUES with type ident = ReflectiveNominal.nominal
                                 and type term = ReflectiveTerm.term
                                 and type pattern = ReflectiveTerm.pattern
                                 and type ('n, 'v) environment = ('n, 'v) ReflectiveEnv.map )    
    (* Algebraic theory of continuations *)
  and ReflectiveK : ( CONTINUATIONS with type nominal = ReflectiveNominal.nominal
                                    and type ('n, 'v) k_env = ('n, 'v) ReflectiveEnv.map )
    (* Algebraic theory of environments *)
  and ReflectiveEnv : ENVIRONMENTS

  type 'a monad = 'a M.monad
      (* The type used for identifiers *)
  type ident = ReflectiveNominal.nominal
      (* The type used for terms in the language *)
  type term = ReflectiveTerm.term
      (* The type used for arithmetic terms in the language *)
  type arith_term = ReflectiveTerm.arithmeticTerm
      (* The type used for patterns in the language *)
  type pattern = ReflectiveTerm.pattern
      (* The type used for list patterns in the language *)
  type lyst = ReflectiveTerm.lyst
      (* The type used for binding declarations in the language *)
  type binding = ReflectiveTerm.binding
      (* The type of literals *)
  type literal = ReflectiveTerm.value
      (* The type of values *)
  type value = ReflectiveValue.value
      (* The type of environments *)
  type env = ReflectiveValue.v_env
      (* The type of continuations *)
  type ktn = ( value, term ) ReflectiveK.cont

  (* The reduction of terms and/or the transitions of the abstract machine *)
  val reduce : term -> env -> ktn -> value monad
  (* The primitive arithmetic operations *)
  val calculate : arith_term -> env -> ktn -> value monad
  (* Application of continuations *)
  val apply_k : ktn -> value -> value
  (* Application of closures *)
  val apply_closure : value -> value -> ktn -> value monad

  (* Pattern-matching *)
  val unify : pattern -> value -> env option
  val materialize : literal -> value

  (* Divergence *)
  val bottom : value
  (* Unit *)
  val yunit : value

  (* Initial configurations *)
  val init_env : env
  val init_k : ktn 
end 

(*
  An abstract machine derived from a monadic evaluator for a language that supports:
  reflection
  delimited continuations
  monadic comprehension
*)
module ReflectiveEval : EVALFUNCTOR =
  functor ( M : MONAD ) ->
struct
  (* This gives a simple and effective form of reflection for quasiquote *)
  module rec ReflectiveNominal : ( NOMINALS with type symbol = Symbols.symbol
                                            and type term = ReflectiveTerm.term )
    (* Algebraic theory of names, variables, identifiers *)
    = NOMINAL( ReflectiveTerm )
  and ReflectiveTerm : ( TERMS with type var = ReflectiveNominal.nominal) 
    (* Algebraic theory of terms *)
    = TERM( ReflectiveNominal ) 
  and ReflectiveValue : ( VALUES with type ident = ReflectiveNominal.nominal
                                 and type term = ReflectiveTerm.term
                                 and type pattern = ReflectiveTerm.pattern
                                 and type ('n, 'v) environment = ('n, 'v) ReflectiveEnv.map )
    (* Algebraic theory of values *)
  = VALUEFUNCTOR( ReflectiveNominal )( ReflectiveTerm )( ReflectiveEnv )
  and ReflectiveK : ( CONTINUATIONS with type nominal = ReflectiveNominal.nominal
                                    and type ('n, 'v) k_env = ('n, 'v) ReflectiveEnv.map )
    (* Algebraic theory of continuations *)
    = CONTINUATIONFUNCTOR( ReflectiveNominal )( ReflectiveEnv )
  and ReflectiveEnv : ENVIRONMENTS =
    (* Algebraic theory of environments *)
    ListEnv (* ReflectiveNominal *)

  type 'a monad = 'a M.monad
  type ident = ReflectiveNominal.nominal
  type term = ReflectiveTerm.term
  type arith_term = ReflectiveTerm.arithmeticTerm
  type pattern = ReflectiveTerm.pattern
  type lyst = ReflectiveTerm.lyst
  type binding = ReflectiveTerm.binding
  type literal = ReflectiveTerm.value
  type value = ReflectiveValue.value
  type env = ReflectiveValue.v_env
  type ktn = ( value, term ) ReflectiveK.cont

  exception NonFunctionInOpPosition of value 
  exception MatchFailure of pattern * value
  exception RuntimeException of string * term
  exception UnboundVariable of ident
      
  let bottom = ReflectiveValue.BOTTOM
  let yunit = ReflectiveValue.UNIT 

  let rec reduce t e k =
    match t with 
        (* sequential composition *)
        ReflectiveTerm.Sequence( [] ) ->
          ( M.m_unit ( apply_k k yunit )  )
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
                      ( apply_closure clsr ReflectiveValue.UNIT k )
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
                              ( apply_closure clsr a k )
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
                    ( Some( ReflectiveValue.Env( ptn_env ) ), ReflectiveValue.Env( renv ) ) ->
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
          ( M.m_bind 
              ( reduce lhs e k )
              ( fun l ->
                ( M.m_bind
                    ( reduce rhs e k )
                    ( fun r ->
                      ( M.m_unit
                          ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( l == r ) ) ) ) ) ) ) ) )
      | ReflectiveTerm.ComparisonLT( lhs, rhs ) ->
          ( M.m_bind 
              ( reduce lhs e k )
              ( fun l ->
                ( M.m_bind
                    ( reduce rhs e k )
                    ( fun r ->
                      match ( l, r ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 < d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( ( float d1 ) < d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 < float( d2 ) ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 < d2 ) ) ) ) ) ) ) ) )
      | ReflectiveTerm.ComparisonGT( lhs, rhs ) ->
          ( M.m_bind 
              ( reduce lhs e k )
              ( fun l ->
                ( M.m_bind
                    ( reduce rhs e k )
                    ( fun r ->
                      match ( l, r ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 > d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( ( float d1 ) > d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 > float( d2 ) ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 > d2 ) ) ) ) ) ) ) ) )
      | ReflectiveTerm.ComparisonLTE( lhs, rhs ) ->
          ( M.m_bind 
              ( reduce lhs e k )
              ( fun l ->
                ( M.m_bind
                    ( reduce rhs e k )
                    ( fun r ->
                      match ( l, r ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 <= d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( ( float d1 ) <= d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 <= float( d2 ) ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 <= d2 ) ) ) ) ) ) ) ) )
      | ReflectiveTerm.ComparisonGTE( lhs, rhs ) ->
          ( M.m_bind 
              ( reduce lhs e k )
              ( fun l ->
                ( M.m_bind
                    ( reduce rhs e k )
                    ( fun r ->
                      match ( l, r ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 >= d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( ( float d1 ) >= d2 ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 >= float( d2 ) ) ) ) ) )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( M.m_unit
                                   ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 >= d2 ) ) ) ) ) ) ) ) )

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
                ( match ( ReflectiveEnv.lookup ( v, renv ) ) with
                    Some( rslt ) ->
                      ( M.m_unit ( apply_k k rslt ) )
                  | _ -> raise ( UnboundVariable v ) )
            | _ -> raise ( NotYetImplemented "Mention wildcard" ) )
      | Actualization( aterm ) ->
          raise ( NotYetImplemented "Actualization" )
      | Aggregation( aterm ) ->
          raise ( NotYetImplemented "Aggregation" )
  and materialize lit = 
    match lit with
        ReflectiveTerm.BooleanLiteral( ReflectiveTerm.Verity ) ->
          ReflectiveValue.Ground( ReflectiveValue.Boolean( true ) )
      | ReflectiveTerm.BooleanLiteral( ReflectiveTerm.Absurdity ) ->
          ReflectiveValue.Ground( ReflectiveValue.Boolean( false ) )
      | ReflectiveTerm.StringLiteral( s ) ->
          ReflectiveValue.Ground( ReflectiveValue.String( s ) )
      | ReflectiveTerm.IntegerLiteral( i ) ->
          ReflectiveValue.Ground( ReflectiveValue.Integer( i ) )
      | ReflectiveTerm.DoubleLiteral( d ) ->
          ReflectiveValue.Ground( ReflectiveValue.Double( d ) )
      | ReflectiveTerm.Reification( t ) ->
          ReflectiveValue.Ground( ReflectiveValue.Reification( t ) )
      | ReflectiveTerm.UNIT -> yunit
  and apply_k k v =
    match ( k, v ) with 
        ( ReflectiveK.STOP, v ) -> v
      | _ -> raise ( NotYetImplemented "apply_k non-STOP k's" )
  and apply_closure op v k =
    match op with
        Closure( c_ptn, c_term, c_env ) ->
          let nc_ptn : pattern = c_ptn in
          ( match ( ( unify nc_ptn v ), c_env ) with
              ( Some( ReflectiveValue.Env( c_ptn_env ) ), ReflectiveValue.Env( c_renv ) ) ->
                let nc_k : ktn = k in
                let nc_term : term = c_term in
                let nc_env : env = ( ReflectiveValue.Env( ReflectiveEnv.sum c_ptn_env c_renv ) ) in
                  ( reduce nc_term nc_env nc_k )
            | _ -> raise ( MatchFailure ( c_ptn, v ) ) )
      | _ -> raise ( NonFunctionInOpPosition op )
  and unify p t = 
    match ( p, t ) with 
        ( ReflectiveTerm.Element( fnctr, sptns ), t ) -> 
          raise ( NotYetImplemented "unify Element" )
      | ( ReflectiveTerm.Variable( ReflectiveTerm.Identifier( n ) ), t ) -> 
          ( Some ( ReflectiveValue.Env ( ReflectiveEnv.extend ( n, t, ( ReflectiveEnv.empty ) ) ) ) )
      | ( ReflectiveTerm.Materialization( c ), t ) -> 
          let m = ( materialize c ) in 
            ( match ( m == t ) with
                true -> ( Some ( ReflectiveValue.Env ( ReflectiveEnv.empty ) ) )
              | false -> raise ( MatchFailure ( p, t ) ) ) 
      | ( ReflectiveTerm.Procession( l ), t ) ->
          raise ( NotYetImplemented "unify Procession" ) 
      | ( ReflectiveTerm.PtnSequence( v ), t ) ->
          raise ( NotYetImplemented "unify PtnSequence" ) 
      | ( ReflectiveTerm.PtnApplication( fn, actls ), t ) -> 
          raise ( NotYetImplemented "unify PtnApplication" )
      | ( ReflectiveTerm.PtnSupposition( lptn, ltrm, lbody ), t ) -> 
          raise ( NotYetImplemented "unify PtnSupposition" )
      | ( ReflectiveTerm.PtnRecurrence( lptn, ltrm, lbody ), t ) -> 
          raise ( NotYetImplemented "unify PtnRecurrence" )
      | ( ReflectiveTerm.PtnAbstraction( ptn, body ), t ) -> 
          raise ( NotYetImplemented "unify PtnRecurrence" )
      | ( ReflectiveTerm.PtnCondition( ptest, ptrue, pfalse ), t ) -> 
          raise ( NotYetImplemented "unify PtnCondition" )
      | ( ReflectiveTerm.PtnComprehension( pbnds, pbdy ), t ) -> 
          raise ( NotYetImplemented "unify PtnComprehension" )
      | ( ReflectiveTerm.PtnConsolidation( pbnds, pbdy ), t ) -> 
          raise ( NotYetImplemented "unify PtnConsolidation" )
      | ( ReflectiveTerm.PtnFiltration( pbnds, pbdy, pcnds ), t ) -> 
          raise ( NotYetImplemented "unify PtnConsolidation" )
      | ( ReflectiveTerm.PtnConcentration( pbnds, pbdy, pcnds ), t ) -> 
          raise ( NotYetImplemented "unify PtnConcentration" )
      | ( ReflectiveTerm.PtnEquation( pl, pr ), t ) -> 
          raise ( NotYetImplemented "unify PtnEquation" )
      | ( ReflectiveTerm.PtnComparisonLT( pl, pr ), t ) -> 
          raise ( NotYetImplemented "unify PtnComparisonLT" )
      | ( ReflectiveTerm.PtnComparisonGT( pl, pr ), t ) -> 
          raise ( NotYetImplemented "unify PtnComparisonGT" )
      | ( ReflectiveTerm.PtnComparisonLTE( pl, pr ), t ) -> 
          raise ( NotYetImplemented "unify PtnComparisonLTE" ) 
      | ( ReflectiveTerm.PtnComparisonGTE( pl, pr ), t ) -> 
          raise ( NotYetImplemented "unify PtnComparisonGTE" ) 
      | ( ReflectiveTerm.PtnReflection( v ), t ) ->
          raise ( NotYetImplemented "unify PtnReflection" ) 
      | ( ReflectiveTerm.PtnAcquisition, t ) ->
          raise ( NotYetImplemented "unify PtnAcquisition" ) 
      | ( ReflectiveTerm.PtnSuspension( u, v ), t ) -> 
          raise ( NotYetImplemented "unify PtnSuspension" )
      | ( ReflectiveTerm.PtnRelease( u, v ), t ) -> 
          raise ( NotYetImplemented "unify PtnRelease" ) 
      | ( ReflectiveTerm.PtnInnerSuspension( u, v ), t ) -> 
          raise ( NotYetImplemented "unify PtnInnerSuspension" ) 
      | ( ReflectiveTerm.PtnDivision( v1, v2 ), t ) -> 
          raise ( NotYetImplemented "unify PtnDivision" ) 
      | ( ReflectiveTerm.PtnAddition( v1, v2 ), t ) -> 
          raise ( NotYetImplemented "unify PtnAddition" ) 
      | ( ReflectiveTerm.PtnMultiplication( v1, v2 ), t ) -> 
          raise ( NotYetImplemented "unify PtnMultiplication" ) 
      | ( ReflectiveTerm.PtnJuxtaposition( v1, v2 ), t ) -> 
          raise ( NotYetImplemented "unify PtnJuxtaposition" ) 
      | ( ReflectiveTerm.PtnNegation( n ), t ) -> 
          raise ( NotYetImplemented "unify PtnNegation" )   

  let init_env = ( ReflectiveValue.Env ReflectiveEnv.empty )
  let init_k = ReflectiveK.STOP
end



