(* -*- mode: Tuareg;-*-  *)
(* Filename:    Evaluator.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Thu Aug 21 10:37:32 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Environments
open Nominals
open Terms
open Values
open Continuations
open Exceptions
open Knot
open Observation
open Monad
open Symbols
open Showoff

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
  module ReductionObservations : STRINGTHEORY
  (* Algebraic theory of names, variables, identifiers *)
  module rec ReflectiveNominal : ( NOMINALS with type symbol = Symbols.symbol
                                            and type term = ReflectiveTerm.term )
    (* Algebraic theory of terms *)
  and ReflectiveTerm : ( TERMS with type var = ReflectiveNominal.nominal
                               and type intrinsic = ReflectiveValue.value )
    (* Algebraic theory of values *)
  and ReflectiveValue : ( VALUES with type ident = ReflectiveNominal.nominal
                                 and type term = ReflectiveTerm.term
                                 and type pattern = ReflectiveTerm.pattern
                                 and type ('n, 'v) environment = ('n, 'v) ReflectiveEnv.map    
                                 and type ('v, 't) continuation = ('v, 't ) ReflectiveK.cont
                                 and type ('v, 't) meta_continuation = ('v, 't ) ReflectiveK.meta_cont )
    (* Algebraic theory of continuations *)
  and ReflectiveK : ( CONTINUATIONS with type nominal = ReflectiveNominal.nominal
                                    and type ('n, 'v) k_env = ('n, 'v) ReflectiveEnv.map )
    (* Algebraic theory of environments *)
  and ReflectiveEnv : ENVIRONMENTS
  and ReflectiveDisplay : ( SHOWOFF with type ident = ReflectiveNominal.nominal
                                    and type term = ReflectiveTerm.term
                                    and type arithmetic_term = ReflectiveTerm.arithmeticTerm
                                    and type pattern = ReflectiveTerm.pattern
                                    and type binding = ReflectiveTerm.binding
                                    and type variation = ReflectiveTerm.variation
                                    and type lyst = ReflectiveTerm.lyst
                                    and type value = ReflectiveValue.value                                    
                                    and type ground = ReflectiveValue.ground
                                    and type environment = ReflectiveValue.v_env
                                    and type continuation = ReflectiveValue.v_ktn
                                    and type meta_continuation =  ReflectiveValue.v_meta_ktn )

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

  (* The type of prompts *)
  type prompt = ReflectiveK.prompt
      
  type meta_ktn = ( value, term ) ReflectiveK.meta_cont

  (* The reduction of terms and/or the transitions of the abstract machine *)
  val reduce : term -> env -> prompt -> ktn -> meta_ktn -> prompt -> value monad
  (* The primitive arithmetic operations *)
  val calculate : arith_term -> env -> prompt -> ktn -> meta_ktn -> prompt -> value monad
  (* Application of continuations *)
  val apply_k : ktn -> value -> prompt -> meta_ktn -> prompt -> value monad
  (* Application of closures *)
  
  (* Pattern-matching *)
  val unify : pattern -> value -> env option
  val materialize : literal -> value

  (* Divergence *)
  val bottom : value
  (* Unit *)
  val yunit : value
    
  val init_env : env
  val init_k : value -> env -> meta_ktn -> prompt -> ktn monad

  val initial_prompt : unit -> prompt
  val the_prompt : prompt ref
  val initial_meta_ktn : unit -> meta_ktn

  val new_prompt : ktn -> meta_ktn -> prompt -> value monad
  val push_prompt : term -> term -> env -> prompt -> ktn -> meta_ktn -> prompt -> value monad
  val with_sub_cont : term -> term -> env -> prompt -> ktn -> meta_ktn -> prompt -> value monad
  val push_sub_cont : term -> term -> env -> prompt -> ktn -> meta_ktn -> prompt -> value monad

  val k_pop : value -> meta_ktn -> prompt -> value monad

  val m_mult : ( 'a monad ) monad -> 'a monad
end 

(* The type of an abstract machine derived from a monadic evaluator *)
module type EVALFUNCTOR =
  functor ( M : MONAD ) ->
sig
  (* This is an interesting test of this approach. In the current
     factorization, M cannot be used to inspect or report on the
     transitions of the machine for the purposes of debugging the
     machine. So, we have to have a separate entity for controlling
     that aspect of execution. *)
  module ReductionObservations : STRINGTHEORY
  (* Algebraic theory of names, variables, identifiers *)
  module rec ReflectiveNominal : ( NOMINALS with type symbol = Symbols.symbol
                                            and type term = ReflectiveTerm.term )
    (* Algebraic theory of terms *)
  and ReflectiveTerm : ( TERMS with type var = ReflectiveNominal.nominal 
                               and type intrinsic = ReflectiveValue.value )
    (* Algebraic theory of values *)
  and ReflectiveValue : ( VALUES with type ident = ReflectiveNominal.nominal
                                 and type term = ReflectiveTerm.term
                                 and type pattern = ReflectiveTerm.pattern
                                 and type ('n, 'v) environment = ('n, 'v) ReflectiveEnv.map 
                                 and type ('v, 't) continuation = ('v, 't ) ReflectiveK.cont
                                 and type ('v, 't) meta_continuation = ('v, 't ) ReflectiveK.meta_cont )
    (* Algebraic theory of continuations *)
  and ReflectiveK : ( CONTINUATIONS with type nominal = ReflectiveNominal.nominal
                                    and type ('n, 'v) k_env = ('n, 'v) ReflectiveEnv.map )
    (* Algebraic theory of environments *)
  and ReflectiveEnv : ENVIRONMENTS
  and ReflectiveDisplay : ( SHOWOFF with type ident = ReflectiveNominal.nominal
                                    and type term = ReflectiveTerm.term
                                    and type arithmetic_term = ReflectiveTerm.arithmeticTerm
                                    and type pattern = ReflectiveTerm.pattern
                                    and type binding = ReflectiveTerm.binding
                                    and type variation = ReflectiveTerm.variation
                                    and type lyst = ReflectiveTerm.lyst
                                    and type value = ReflectiveValue.value
                                    and type ground = ReflectiveValue.ground
                                    and type environment = ReflectiveValue.v_env
                                    and type continuation = ReflectiveValue.v_ktn
                                    and type meta_continuation =  ReflectiveValue.v_meta_ktn )

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

  (* The type of prompts *)
  type prompt = ReflectiveK.prompt

  (* The type of meta-continuations *)
  type meta_ktn = ( value, term ) ReflectiveK.meta_cont

  (* The reduction of terms and/or the transitions of the abstract machine *)
  val reduce : term -> env -> prompt -> ktn -> meta_ktn -> prompt -> value monad
  (* The primitive arithmetic operations *)
  val calculate : arith_term -> env -> prompt -> ktn -> meta_ktn -> prompt -> value monad
  (* Application of continuations *)
  val apply_k : ktn -> value -> prompt -> meta_ktn -> prompt -> value monad
  (* Application of closures *)
  
  (* Pattern-matching *)
  val unify : pattern -> value -> env option
  val materialize : literal -> value

  (* Divergence *)
  val bottom : value
  (* Unit *)
  val yunit : value

  (* Initial configurations *)
  val init_env : env
  val init_k : value -> env -> meta_ktn -> prompt -> ktn monad

  val initial_prompt : unit -> prompt
  val the_prompt : prompt ref
  val initial_meta_ktn : unit -> meta_ktn

  val new_prompt : ktn -> meta_ktn -> prompt -> value monad
  val push_prompt : term -> term -> env -> prompt -> ktn -> meta_ktn -> prompt -> value monad
  val with_sub_cont : term -> term -> env -> prompt -> ktn -> meta_ktn -> prompt -> value monad
  val push_sub_cont : term -> term -> env -> prompt -> ktn -> meta_ktn -> prompt -> value monad

  val k_pop : value -> meta_ktn -> prompt -> value monad

  val m_mult : ( 'a monad ) monad -> 'a monad
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
  module ReductionObservations : STRINGTHEORY = OBSERVER
  (* This gives a simple and effective form of reflection for quasiquote *)
  module rec ReflectiveNominal : ( NOMINALS with type symbol = Symbols.symbol
                                            and type term = ReflectiveTerm.term )
    (* Algebraic theory of names, variables, identifiers *)
    = NOMINAL( ReflectiveTerm )
  and ReflectiveTerm : ( TERMS with type var = ReflectiveNominal.nominal
                               and type intrinsic = ReflectiveValue.value )
    (* Algebraic theory of terms *)
    = TERM( ReflectiveNominal )( ReflectiveValue )
  and ReflectiveValue : ( VALUES with type ident = ReflectiveNominal.nominal
                                 and type term = ReflectiveTerm.term
                                 and type pattern = ReflectiveTerm.pattern
                                 and type ('n, 'v) environment = ('n, 'v) ReflectiveEnv.map 
                                 and type ('v, 't) continuation = ('v, 't ) ReflectiveK.cont
                                 and type ('v, 't) meta_continuation = ('v, 't ) ReflectiveK.meta_cont )
    (* Algebraic theory of values *)
    = VALUEFUNCTOR( ReflectiveNominal )( ReflectiveTerm )( ReflectiveEnv )( ReflectiveK )
  and ReflectiveK : ( CONTINUATIONS with type nominal = ReflectiveNominal.nominal
                                    and type ('n, 'v) k_env = ('n, 'v) ReflectiveEnv.map )
    (* Algebraic theory of continuations *)
    = CONTINUATIONFUNCTOR( ReflectiveNominal )( ReflectiveEnv )
  and ReflectiveEnv : ENVIRONMENTS =
    (* Algebraic theory of environments *)
    ListEnv (* ReflectiveNominal *)
  and ReflectiveDisplay : ( SHOWOFF with type ident = ReflectiveNominal.nominal 
                                    and type term = ReflectiveTerm.term
                                    and type arithmetic_term = ReflectiveTerm.arithmeticTerm
                                    and type pattern = ReflectiveTerm.pattern
                                    and type binding = ReflectiveTerm.binding
                                    and type variation = ReflectiveTerm.variation
                                    and type lyst = ReflectiveTerm.lyst
                                    and type value = ReflectiveValue.value
                                    and type ground = ReflectiveValue.ground
                                    and type environment = ReflectiveValue.v_env
                                    and type continuation = ReflectiveValue.v_ktn
                                    and type meta_continuation =  ReflectiveValue.v_meta_ktn )
    = ShowOffFunctor( ReflectiveNominal )( ReflectiveTerm )( ReflectiveValue )

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

  (* The type of prompts *)
  type prompt = ReflectiveK.prompt
  (* The type of meta-continuations *)
  type meta_ktn = ( value, term ) ReflectiveK.meta_cont

  exception NonFunctionInOpPosition of value 
  exception MatchFailure of pattern * value
  exception RuntimeException of string * term
  exception UnboundVariable of ident
  exception PushPromptPromptTypeError of value
  exception FunPushPromptMatchError of meta_ktn
      
  let split_meta_k p m = 
    let rec loop l acc = 
      match l with
          [] -> ( acc, [] )
        | ReflectiveK.Prompt( pp ) :: r ->
            if ( pp == p )
            then ( acc, r )
            else ( loop r ( List.append acc [ ( ReflectiveK.Prompt pp ) ] ) )
        | ReflectiveK.K( kp ) :: r ->
            ( loop r ( List.append acc [ ReflectiveK.K( kp ) ] ) ) in
      ( loop m [] )

  let bottom = ReflectiveValue.BOTTOM
  let yunit = ReflectiveValue.UNIT     

  let observation_context =
    ReductionObservations.observation_context()
  let report_p () =
    ( ReductionObservations.report_reductions_p observation_context ) 
  let show_role show_fn t =
    let s = ( show_fn t ) in
    let init_size = 16 in (* you may want to adjust this *)
    let b = Buffer.create init_size in
      s b;
      Buffer.contents b
  let show t =
    show_role ReflectiveDisplay.show_term t

  let show_hyp t =
    show_role ReflectiveDisplay.show_hypothesis t
      
  let show_cnsq t =
    show_role ReflectiveDisplay.show_consequent t

  let show_trans t =
    show_role ReflectiveDisplay.show_transition t

  let rec reduce t e p k m q =
    (
      if ( report_p() ) 
      then 
        ( print_string ( show_trans t ) )
    );
    match t with 
        (* sequential composition *)
        ReflectiveTerm.Sequence( [] ) ->          
          ( apply_k k yunit p m q )
      | ReflectiveTerm.Sequence( thd :: ttl ) -> 
          let _ = ( reduce thd e p k m q ) in 
          let rec loop ts =
            match ts with
                tshd :: [] -> ( reduce tshd e p k m q )
              | tshd :: tstl ->
                  let _ = ( reduce tshd e p k m q ) in
                    ( loop tstl )          
              | _ -> raise NotEnough
          in ( loop ttl )

      (* application *)
      | ReflectiveTerm.Application( op, [] ) ->
          ( match e with
              ReflectiveValue.Env( renv ) ->
                let nk : ktn =
                  ( ReflectiveK.ARG
                      ( ( ReflectiveTerm.Calculation
                            ( ReflectiveTerm.Actualization ReflectiveTerm.UNIT ) ),
                      renv,
                      k,
                      m,
                      q ) ) in
                  ( reduce op e p nk m q ) )

      | ReflectiveTerm.Application( op, actls ) ->          
          ( match e with
            ReflectiveValue.Env( renv ) ->
              let arg_reduce ( actual : term ) ( acc : ktn ) =
                ( ReflectiveK.ARG ( actual, renv, acc, m, q ) ) in
              let nk : ktn =
                ( List.fold_right arg_reduce actls k ) in
                ( reduce op e p nk m q ) )

      (* let *)
      | ReflectiveTerm.Supposition( ptn, pterm, eterm ) ->
          ( M.m_bind
              ( reduce pterm e p k m q )
              ( fun a ->                
                match ( ( unify ptn a ), e ) with
                    ( Some( ReflectiveValue.Env( ptn_env ) ), ReflectiveValue.Env( renv ) ) ->
                      ( reduce
                          eterm
                          ( ReflectiveValue.Env( ReflectiveEnv.sum ptn_env renv ) )
                          p
                          k 
                          m
                          q )
                  | _ -> raise ( MatchFailure ( ptn, a ) )
              )
          )

      (* letrec *)
      | ReflectiveTerm.Recurrence( ptn, pterm, eterm ) ->
          raise ( NotYetImplemented "Recurrence" )

      (* abstraction *)
      | ReflectiveTerm.Abstraction( ptn, eterm ) ->
          ( apply_k k ( ReflectiveValue.Closure( ptn, eterm, e ) ) p m q )
            
      (* condition *)            
      | ReflectiveTerm.Condition( test, tbranch, fbranch ) ->
          ( M.m_bind
              ( reduce test e p k m q )
              ( fun a ->
                ( match a with
                    ReflectiveValue.Ground( ReflectiveValue.Boolean( true ) ) -> ( reduce tbranch e p k m q )
                  | ReflectiveValue.Ground( ReflectiveValue.Boolean( false ) ) -> ( reduce fbranch e p k m q )
                  | _ -> raise ( RuntimeException ( "expected Boolean", test ) ) ) ) )

      (* monadic desugaring *)
      (*  This has been moved to the syntactic transform stage *)

      (* comparison *)
      | ReflectiveTerm.Equation( lhs, rhs ) ->
          ( M.m_bind 
              ( reduce lhs e p k m q )
              ( fun l ->
                ( M.m_bind
                    ( reduce rhs e p k m q )
                    ( fun r ->
                      ( apply_k
                          k
                          ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( l == r ) ) )
                          p
                          m
                          q ) ) ) ) )
      | ReflectiveTerm.ComparisonLT( lhs, rhs ) ->
          ( M.m_bind 
              ( reduce lhs e p k m q )
              ( fun l ->
                ( M.m_bind
                    ( reduce rhs e p k m q )
                    ( fun r ->
                      match ( l, r ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k
                                   k
                                   ( ReflectiveValue.Ground ( ReflectiveValue.Boolean ( d1 < d2 ) ) )
                                   p
                                   m 
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean ( ( float d1 ) < d2 ) ) )
                                   p
                                   m 
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean
                                           ( d1 < float( d2 ) ) ) )
                                   p
                                   m 
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                           ( ReflectiveValue.Boolean
                                               ( d1 < d2 ) ) )
                                   p
                                   m
                                   q ) ) ) ) )
      | ReflectiveTerm.ComparisonGT( lhs, rhs ) ->
          ( M.m_bind 
              ( reduce lhs e p k m q )
              ( fun l ->
                ( M.m_bind
                    ( reduce rhs e p k m q )
                    ( fun r ->
                      match ( l, r ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean
                                           ( d1 > d2 ) ) )
                                   p
                                   m 
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean
                                           ( ( float d1 ) > d2 ) ) )
                                   p
                                   m 
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean
                                           ( d1 > float( d2 ) ) ) )
                                   p
                                   m 
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean ( d1 > d2 ) ) )
                                   p
                                   m 
                                   q ) ) ) ) )
      | ReflectiveTerm.ComparisonLTE( lhs, rhs ) ->
          ( M.m_bind 
              ( reduce lhs e p k m q )
              ( fun l ->
                ( M.m_bind
                    ( reduce rhs e p k m q )
                    ( fun r ->
                      match ( l, r ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean ( d1 <= d2 ) ) )
                                   p
                                   m
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean
                                           ( ( float d1 ) <= d2 ) ) )
                                   p
                                   m
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                                                      
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean
                                           ( d1 <= float( d2 ) ) ) )
                                   p
                                   m
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean
                                           ( d1 <= d2 ) ) )
                                   p
                                   m
                                   q ) ) ) ) )
      | ReflectiveTerm.ComparisonGTE( lhs, rhs ) ->
          ( M.m_bind 
              ( reduce lhs e p k m q )
              ( fun l ->
                ( M.m_bind
                    ( reduce rhs e p k m q )
                    ( fun r ->
                      match ( l, r ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean
                                           ( d1 >= d2 ) ) )
                                   p
                                   m
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean
                                           ( ( float d1 ) >= d2 ) ) )
                                   p
                                   m 
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                                                      
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean
                                           ( d1 >= float( d2 ) ) ) )
                                   p
                                   m
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                  
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Boolean
                                           ( d1 >= d2 ) ) )
                                   p
                                   m
                                   q ) ) ) ) )

      (* reflection -- dual to reification *)
      | ReflectiveTerm.Reflection( v ) ->
          raise ( NotYetImplemented "Reflection" )

      (* delimited continuations *)
      | ReflectiveTerm.Acquisition -> 
          ( new_prompt k m q )
      | ReflectiveTerm.Suspension( pterm, eterm ) -> 
          ( push_prompt pterm eterm e p k m q )
      | ReflectiveTerm.Release( pterm, eterm ) -> 
          ( with_sub_cont pterm eterm e p k m q )
      | ReflectiveTerm.InnerSuspension( pterm, eterm ) -> 
          ( push_sub_cont pterm eterm e p k m q )

      (* primitive arithmetic calculation *)
      | ReflectiveTerm.Calculation( aterm ) -> 
          ( calculate aterm e p k m q )
  and calculate a e p k m q =
    match a with 
        ReflectiveTerm.Division( aterm1, aterm2 ) ->
          ( M.m_bind
              ( calculate aterm1 e p k m q )
              ( fun a ->
                ( M.m_bind
                    ( calculate aterm2 e p k m q )
                    ( fun b ->
                      match ( a, b ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Double
                                           ( d1 /. d2 ) ) )
                                   p
                                   m
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Double
                                           ( ( float d1 ) /. d2 ) ) )
                                   p
                                   m
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Double
                                           ( d1 /. float( d2 ) ) ) )
                                   p
                                   m
                                   q )
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Integer
                                           ( d1 / d2 ) ) )
                                   p
                                   m
                                   q )
                    )
                )
              )
          )
            
      | ReflectiveTerm.Addition( aterm1, aterm2 ) ->
          ( M.m_bind
              ( calculate aterm1 e p k m q )
              ( fun a ->
                ( M.m_bind
                    ( calculate aterm2 e p k m q )
                    ( fun b ->
                      match ( a, b ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Double
                                           ( d1 +. d2 ) ) )
                                   p
                                   m
                                   q ) 
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Double
                                           ( ( float d1 ) +. d2 ) ) )
                                   p
                                   m
                                   q ) 
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Double
                                           ( d1 +. float( d2 ) ) ) )
                                   p
                                   m
                                   q ) 
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Integer ( d1 + d2 ) ) )
                                   p
                                   m 
                                   q )
                    )
                )
              )
          )
      | ReflectiveTerm.Multiplication( aterm1, aterm2 ) ->
          ( M.m_bind
              ( calculate aterm1 e p k m q )
              ( fun a ->
                ( M.m_bind
                    ( calculate aterm2 e p k m q )
                    ( fun b ->
                      match ( a, b ) with
                          (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                           ( ReflectiveValue.Double
                                               ( d1 *. d2 ) ) )
                                   p
                                   m
                                   q ) 
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Double( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Double
                                           ( ( float d1 ) *. d2 ) ) )
                                   p
                                   m
                                   q ) 
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Double( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Double
                                           ( d1 *. float( d2 ) ) ) )
                                   p
                                   m
                                   q ) 
                        | (
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d1 ) ),
                            ReflectiveValue.Ground( ReflectiveValue.Integer( d2 ) )
                          ) -> ( apply_k                                   
                                   k
                                   ( ReflectiveValue.Ground
                                       ( ReflectiveValue.Integer
                                           ( d1 * d2 ) ) )
                                   p
                                   m
                                   q ) 
                    )
                )
              )
          )
      | ReflectiveTerm.Juxtaposition( aterm1, aterm2 ) ->
          raise ( NotYetImplemented "Juxtaposition" )
      | ReflectiveTerm.Negation( aterm ) ->
          raise ( NotYetImplemented "Negation" )
      | ReflectiveTerm.Mention( n ) ->
          ( match ( n, e ) with
              ( ReflectiveTerm.Identifier( v ), ReflectiveValue.Env( renv ) ) ->
                ( match ( ReflectiveEnv.lookup ( v, renv ) ) with
                    Some( rslt ) ->
                      ( apply_k k rslt p m q )
                  | _ -> raise ( UnboundVariable v ) )
            | _ -> raise ( NotYetImplemented "Mention wildcard" ) )
      | ReflectiveTerm.Actualization( aterm ) ->
          ( apply_k k ( materialize aterm ) p m q )
      | ReflectiveTerm.Aggregation( aterm ) ->
          ( reduce aterm e p k m q )
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
      | ReflectiveTerm.Intrinsic( i ) -> i
      | ReflectiveTerm.UNIT -> yunit
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
  and apply_k k v p m q =
    ( match ( k, v ) with 
        ( ReflectiveK.STOP, v ) -> ( M.m_unit v )
      | (
          ReflectiveK.FUN(
            ReflectiveValue.Closure( ptn, t, e ), kp, mp, qp
          ),
          v
        ) ->
          ( match ( ( unify ptn v ), e ) with
              ( Some( ReflectiveValue.Env( ptn_env ) ), ReflectiveValue.Env( renv ) ) ->
                ( reduce
                    t
                    ( ReflectiveValue.Env( ReflectiveEnv.sum ptn_env renv ) )
                    p
                    k 
                    m
                    q )
            | _ -> raise ( MatchFailure ( ptn, v ) ) )
      | ( ReflectiveK.ARG( t, renv, kp, mp, qp ), v ) ->
          ( reduce
              t
              ( ReflectiveValue.Env renv )
              p
              ( ReflectiveK.FUN ( v, kp, mp, qp ) )
              m
              q )
      (* PUSHPROMPT FUN/ARG pair *)
      | ( 
          ReflectiveK.PUSHPROMPT_ARG( t, renv, kp, mp, qp ),
          ReflectiveValue.Ground( ReflectiveValue.Integer( n ) )
        ) ->
          let nkp = ( ReflectiveK.K kp ) in
          let pv = ( ReflectiveK.Prompt n ) in
            ( reduce
                t
                ( ReflectiveValue.Env renv )
                p
                ( ReflectiveK.PUSHPROMPT_FUN ( n, renv, kp, mp, qp ) )
                m
                q )
      | ( ReflectiveK.PUSHPROMPT_ARG( t, renv, kp, mp, qp ), v ) ->
          raise ( PushPromptPromptTypeError v )
      | ( ReflectiveK.PUSHPROMPT_FUN( n, renv, kp, mp, qp ), v ) ->
          let pp = ( ReflectiveK.Prompt n ) in
          let nkp = ( ReflectiveK.K kp ) in 
          let mpp = ( pp :: ( nkp :: mp ) ) in
            ( M.m_bind
                ( init_k v ( ReflectiveValue.Env renv ) mp q )
                ( fun k0 -> ( apply_k k0 v p mpp q ) ) )

      (* WITHSUBCONT FUN/ARG pair *)
      | (
          ReflectiveK.WITHSUBCONT_ARG( t, renv, kp, mp, qp ),
          ReflectiveValue.Ground( ReflectiveValue.Integer( n ) )
        ) ->
          ( reduce
              t
              ( ReflectiveValue.Env renv )
              p
              ( ReflectiveK.WITHSUBCONT_FUN ( n, renv, kp, mp, qp ) )
              m
              q )
      | (
          ReflectiveK.WITHSUBCONT_FUN( n, renv, kp, mp, qp ),
          ReflectiveValue.Closure( ptn, t, e )
        ) ->
          let ( ml, mr ) = ( split_meta_k n m ) in
          let mk = ( ReflectiveValue.MCont ( ReflectiveValue.MK ml ) ) in
          ( match ( ( unify ptn mk ), e ) with
              ( Some( ReflectiveValue.Env( ptn_env ) ), ReflectiveValue.Env( renv ) ) ->
                ( M.m_bind
                    ( init_k v ( ReflectiveValue.Env renv ) mp q )
                    ( fun k0 ->
                      ( reduce
                          t
                          ( ReflectiveValue.Env( ReflectiveEnv.sum ptn_env renv ) )
                          p
                          k0 
                          mr
                          q ) ) )
            | _ -> raise ( MatchFailure ( ptn, v ) ) )

      (* PUSHSUBCONT FUN/ARG pair *)
      | (
          ReflectiveK.PUSHSUBCONT_ARG( t, renv, kp, mp, qp ),
          ReflectiveValue.MCont( ReflectiveValue.MK( mk ) )
        ) ->
          ( reduce
              t
              ( ReflectiveValue.Env renv )
              p
              k
              mk
              q )
      | (
          ReflectiveK.PUSHSUBCONT_FUN(
            mk, renv, kp, mp, qp
          ),
          v
        ) ->
          let nkp = ( ReflectiveK.K kp ) in 
            ( M.m_bind
                ( init_k v ( ReflectiveValue.Env renv ) mp q )
                ( fun k0 -> ( apply_k k0 v p ( List.append mk ( nkp :: m ) ) q ) ) )
      | _ -> raise ( NotYetImplemented "apply_k non-STOP/FUN/ARG k's" ) )
  and new_prompt k m q =
    ( apply_k k ( ReflectiveValue.Ground ( ReflectiveValue.Integer q ) ) q m ( q + 1 ) )
  and push_prompt t1 t2 e p k m q = 
    ( match e with
        ReflectiveValue.Env( renv ) ->
          let nk = 
            ( ReflectiveK.PUSHPROMPT_ARG ( t2, renv, k, m, q ) )
          in
            ( reduce t1 e p nk m q ) )
  and with_sub_cont t1 t2 e p k m q =
    ( match e with
        ReflectiveValue.Env( renv ) ->
          let nk = 
            ( ReflectiveK.WITHSUBCONT_ARG ( t2, renv, k, m, q ) )
          in
            ( reduce t1 e p nk m q ) )
  and push_sub_cont t1 t2 e p k m q = 
    ( match e with
        ReflectiveValue.Env( renv ) ->
          let nk = 
            ( ReflectiveK.PUSHSUBCONT_ARG ( t2, renv, k, m, q ) )
          in
            ( reduce t1 e p nk m q ) )
  and k_pop v m q =
    match m with
        [] -> ( M.m_unit v )
      | ReflectiveK.Prompt( p ) :: mp ->
          ( k_pop v mp q )
      | ReflectiveK.K( k ) :: mp ->
          (* BUGBUG -- lgm : not sure q is the right value for prompt *)
          ( apply_k k v q mp q )              
  and init_k v e m q =
    let kv : value monad = ( k_pop v m q ) in
    let k_stop : ktn = ReflectiveK.STOP in
      ( match e with
          ReflectiveValue.Env( renv ) ->
            ( M.m_bind
                kv
                ( fun rv ->
                  ( M.m_unit
                      ( ReflectiveK.FUN
                          ( rv, k_stop, m, q ) ) ) ) ) )

  and init_env = ( ReflectiveValue.Env ReflectiveEnv.empty ) 

  let initial_prompt () = 0
  let the_prompt = ref ( initial_prompt() )
  let initial_meta_ktn () = []

  let m_mult mma =
    ( M.m_bind mma ( fun ma -> ma ) )
end



