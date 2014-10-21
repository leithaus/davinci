(* -*- mode: Tuareg;-*-  *)
(* Filename:    Showoff.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Wed Oct  1 12:29:15 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Showcacao
open Nominals
open Terms
open Values
open Symbols
open Exceptions

module type SHOWOFF =
sig
  type ident
  type term
  type arithmetic_term
  type pattern
  type binding
  type variation
  type lyst
  type value
  type ground
  type environment
  type continuation
  type meta_continuation
  type prompt

  val show_ident : ident -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_term : term -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_arithmetic_term : arithmetic_term -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_binding : binding -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_pattern : pattern -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_variation : variation -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_lyst : lyst -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_value : value -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_ground : ground -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_env : environment -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_k : continuation -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_mk : meta_continuation -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_prompt : prompt -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable

  val show_hypothesis : term -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_consequent : term -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_transition : term -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
end  

module type SHOWOFFFUNCTOR =
  functor( Nominals : NOMINALS with type symbol = Symbols.symbol ) ->
    functor( Terms : TERMS with type var = Nominals.nominal ) ->
      functor( Values : VALUES with type term = Terms.term and type prompt = int ) ->
sig
  type ident = Nominals.nominal
  type term = Terms.term
  type arithmetic_term = Terms.arithmeticTerm
  type pattern = Terms.pattern
  type binding = Terms.binding      
  type variation = Terms.variation
  type lyst = Terms.lyst
  type value = Values.value
  type ground = Values.ground
  type environment = Values.v_env
  type continuation = Values.v_ktn
  type meta_continuation = Values.v_meta_ktn
  type prompt = Values.v_prompt

  val show_ident : ident -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_term : term -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_arithmetic_term : arithmetic_term -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_binding : binding -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_pattern : pattern -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_variation : variation -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_lyst : lyst -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_value : value -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_ground : ground -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_env : environment -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_k : continuation -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_mk : meta_continuation -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_prompt : prompt -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable

  val show_hypothesis : term -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_consequent : term -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
  val show_transition : term -> environment -> prompt -> continuation -> meta_continuation -> prompt -> showable
end  

module ShowOffFunctor : SHOWOFFFUNCTOR =
  functor( Nominals : NOMINALS with type symbol = Symbols.symbol ) ->
    functor( Terms : TERMS with type var = Nominals.nominal ) ->
      functor( Values : VALUES with type term = Terms.term and type prompt = int ) ->
struct
  type ident = Nominals.nominal
  type term = Terms.term
  type arithmetic_term = Terms.arithmeticTerm
  type pattern = Terms.pattern
  type binding = Terms.binding      
  type variation = Terms.variation
  type lyst = Terms.lyst
  type value = Values.value
  type ground = Values.ground
  type environment = Values.v_env
  type continuation = Values.v_ktn
  type meta_continuation = Values.v_meta_ktn
  type prompt = Values.v_prompt

  let show_ground g e p k mk q =
    match g with
        Values.Boolean( true ) -> s2s "true"
      | Values.Boolean( false ) -> s2s "false"
      | Values.String( s ) -> ( s2s "\"" >> s2s s >> s2s "\"" )
      | Values.Integer( i ) -> ( showInt i )
      | Values.Double( d ) -> ( showFloat d )
      | Values.Reification( t ) ->
          raise ( NotYetImplemented "render reification" )
  let show_env e env p k mk q = s2s "#<environment>"
  let show_k k e p k mk q = s2s "#<closure>"
  let show_mk mk e p k mk q = s2s "#<closure>"
  let show_prompt prmpt e p k mk q = 
    match prmpt with
        Values.P( i ) -> showInt i
  let show_value v e p k mk q =
    match v with
        Values.Ground( g ) -> show_ground g e p k mk q
      | Values.Closure( p, t, e ) -> s2s "#<closure>"
      | Values.Cont( k ) -> show_k k e p k mk q
      | Values.MCont( mk ) -> show_mk mk e p k mk q
      | Values.BOTTOM ->
          raise ( NotYetImplemented "render bottom" )
      | Values.UNIT -> s2s "()"  

  let rec show_term t e p k mk q : showable =
    match t with
      Terms.Sequence ( terms ) ->
        ( s2s "Sequence" >> c2s ' ' >> c2s '(' >> showList ( fun nt -> ( show_term nt e p k mk q ) ) terms >> c2s ')' )
        |    Terms.Application (term, terms) -> s2s "Application" >> c2s ' ' >> c2s '(' >> show_term term e p k mk q  >> s2s ", " >> showList ( fun nt -> ( show_term nt e p k mk q ) ) terms >> c2s ')'
        |    Terms.Supposition (pattern, term0, term) -> s2s "Supposition" >> c2s ' ' >> c2s '(' >> show_pattern pattern e p k mk q  >> s2s ", " >>  show_term term0 e p k mk q >> s2s ", " >> show_term term e p k mk q >> c2s ')'
        |    Terms.Recurrence (pattern, term0, term) -> s2s "Recurrence" >> c2s ' ' >> c2s '(' >> show_pattern pattern e p k mk q  >> s2s ", " >>  show_term term0 e p k mk q  >> s2s ", " >> show_term term e p k mk q >> c2s ')'
        |    Terms.Abstraction (pattern, term) -> s2s "Abstraction" >> c2s ' ' >> c2s '(' >> show_pattern pattern e p k mk q  >> s2s ", " >> show_term term e p k mk q >> c2s ')'
        |    Terms.Condition (term0, term1, term) -> s2s "Condition" >> c2s ' ' >> c2s '(' >> show_term term0 e p k mk q  >> s2s ", " >>  show_term term1 e p k mk q  >> s2s ", " >> show_term term e p k mk q >> c2s ')'
        |    Terms.Comprehension (bindings, term) -> s2s "Comprehension" >> c2s ' ' >> c2s '(' >> showList ( fun nb -> show_binding nb e p k mk q ) bindings  >> s2s ", " >>  show_term term e p k mk q >> c2s ')'
        |    Terms.Consolidation (bindings, term) -> s2s "Consolidation" >> c2s ' ' >> c2s '(' >> showList ( fun nb -> show_binding nb e p k mk q ) bindings  >> s2s ", " >>  show_term term e p k mk q >> c2s ')'
        |    Terms.Filtration (bindings, patterns, term) -> s2s "Filtration" >> c2s ' ' >> c2s '(' >> showList ( fun nb -> show_binding nb e p k mk q ) bindings  >> s2s ", " >>  showList ( fun nptn -> show_pattern nptn e p k mk q ) patterns  >> s2s ", " >>  show_term term e p k mk q >> c2s ')'
        |    Terms.Concentration (bindings, patterns, term) -> s2s "Concentration" >> c2s ' ' >> c2s '(' >> showList ( fun nb -> show_binding nb e p k mk q ) bindings  >> s2s ", " >>  showList ( fun nptn -> show_pattern nptn e p k mk q ) patterns  >> s2s ", " >>  show_term term e p k mk q >> c2s ')'
        |    Terms.Equation (term0, term) -> s2s "Equation" >> c2s ' ' >> c2s '(' >> show_term term0 e p k mk q  >> s2s ", " >>  show_term term e p k mk q >> c2s ')'
        |    Terms.ComparisonLT (term0, term) -> s2s "ComparisonLT" >> c2s ' ' >> c2s '(' >> show_term term0 e p k mk q  >> s2s ", " >>  show_term term e p k mk q >> c2s ')'
        |    Terms.ComparisonGT (term0, term) -> s2s "ComparisonGT" >> c2s ' ' >> c2s '(' >> show_term term0 e p k mk q  >> s2s ", " >>  show_term term e p k mk q >> c2s ')'
        |    Terms.ComparisonLTE (term0, term) -> s2s "ComparisonLTE" >> c2s ' ' >> c2s '(' >> show_term term0 e p k mk q  >> s2s ", " >>  show_term term e p k mk q >> c2s ')'
        |    Terms.ComparisonGTE (term0, term) -> s2s "ComparisonGTE" >> c2s ' ' >> c2s '(' >> show_term term0 e p k mk q  >> s2s ", " >>  show_term term e p k mk q >> c2s ')'
    |    Terms.Reflection tvar -> s2s "Reflection" >> c2s ' ' >> c2s '(' >> show_ident tvar e p k mk q >> c2s ')'
    |    Terms.Acquisition  -> s2s "Acquisition" 
    |    Terms.Suspension (term0, term) -> s2s "Suspension" >> c2s ' ' >> c2s '(' >> show_term term0 e p k mk q  >> s2s ", " >> show_term term e p k mk q >> c2s ')'
    |    Terms.Release (term0, term) -> s2s "Release" >> c2s ' ' >> c2s '(' >> show_term term0 e p k mk q  >> s2s ", " >> show_term term e p k mk q >> c2s ')'
    |    Terms.InnerSuspension (term0, term) -> s2s "InnerSuspension" >> c2s ' ' >> c2s '(' >> show_term term0 e p k mk q  >> s2s ", " >>  show_term term e p k mk q >> c2s ')'
    |    Terms.Calculation arithmeticterm -> s2s "Calculation" >> c2s ' ' >> c2s '(' >> show_arithmetic_term arithmeticterm e p k mk q >> c2s ')'


  and show_arithmetic_term t e p k mk q : showable =
    match t with
        Terms.Division (arithmeticterm0, arithmeticterm) -> s2s "Division" >> c2s ' ' >> c2s '(' >> show_arithmetic_term arithmeticterm0 e p k mk q  >> s2s ", " >> show_arithmetic_term arithmeticterm e p k mk q >> c2s ')'
      | Terms.Addition (arithmeticterm0, arithmeticterm) -> s2s "Addition" >> c2s ' ' >> c2s '(' >> show_arithmetic_term arithmeticterm0 e p k mk q  >> s2s ", " >> show_arithmetic_term arithmeticterm e p k mk q >> c2s ')'
      | Terms.Multiplication (arithmeticterm0, arithmeticterm) -> s2s "Multiplication" >> c2s ' ' >> c2s '(' >> show_arithmetic_term arithmeticterm0 e p k mk q  >> s2s ", " >>  show_arithmetic_term arithmeticterm e p k mk q >> c2s ')'
      | Terms.Juxtaposition (arithmeticterm0, arithmeticterm) -> s2s "Juxtaposition" >> c2s ' ' >> c2s '(' >> show_arithmetic_term arithmeticterm0 e p k mk q  >> s2s ", " >>  show_arithmetic_term arithmeticterm e p k mk q >> c2s ')'
      | Terms.Negation arithmeticterm -> s2s "Negation" >> c2s ' ' >> c2s '(' >> show_arithmetic_term arithmeticterm e p k mk q >> c2s ')'
      | Terms.Mention vr ->
          let sv : showable = ( show_variation vr e p k mk q ) in
            s2s "Mention" >> c2s ' ' >> c2s '(' >> sv >> c2s ')'
      | Terms.Actualization value -> s2s "Actualization" >> c2s ' ' >> c2s '(' >> show_literal value e p k mk q >> c2s ')'
      | Terms.Aggregation term -> s2s "Aggregation" >> c2s ' ' >> c2s '(' >> show_term term e p k mk q >> c2s ')'


  and show_binding b e p k mk q : showable =
    match b with
        Terms.Question (pattern, term) -> s2s "Question" >> c2s ' ' >>
          c2s '(' >> show_pattern pattern e p k mk q  >> s2s ", " >>  show_term term e p k mk q >> c2s ')'


  and show_pattern ptn e p k mk q : showable =
    match ptn with
        Terms.Element (symbol, patterns) -> s2s "Element" >> c2s ' ' >> c2s '(' >> show_symbol symbol  >> s2s ", " >>  showList ( fun nptn -> show_pattern nptn e p k mk q ) patterns >> c2s ')'
    |    Terms.Variable tvar ->
           s2s "Variable" >> c2s ' ' >> c2s '(' >> show_variation tvar e p k mk q >> c2s ')'
    |    Terms.Materialization value -> s2s "Materialization" >> c2s ' ' >> c2s '(' >> show_literal value e p k mk q >> c2s ')'
    |    Terms.Procession lyst -> s2s "Procession" >> c2s ' ' >> c2s '(' >> show_lyst lyst e p k mk q >> c2s ')' 
    |    Terms.PtnSequence tvar -> s2s "PtnSequence" >> c2s ' ' >> c2s '(' >> show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnApplication (tvar0, tvar) -> s2s "PtnApplication" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q >> s2s ", " >> show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnSupposition (tvar0, tvar1, tvar) -> s2s "PtnSupposition" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q >> s2s ", " >>  show_ident tvar1 e p k mk q >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnRecurrence (tvar0, tvar1, tvar) -> s2s "PtnRecurrence" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q >> s2s ", " >>  show_ident tvar1 e p k mk q >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnAbstraction (tvar0, tvar) -> s2s "PtnAbstraction" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnCondition (tvar0, tvar1, tvar) -> s2s "PtnCondition" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar1 e p k mk q  >> s2s ", " >> show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnComprehension (tvar0, tvar) -> s2s "PtnComprehend" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnConsolidation (tvar0, tvar) -> s2s "PtnConsolidate" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnFiltration (tvar0, tvar1, tvar) -> s2s "PtnFiltration" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q           >> s2s ", " >>  show_ident tvar1 e p k mk q >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnConcentration (tvar0, tvar1, tvar) -> s2s "PtnConcentrate" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar1 e p k mk q  >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnEquation (tvar0, tvar) -> s2s "PtnEquation" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >> show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnComparisonLT (tvar0, tvar) -> s2s "PtnCompLT" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnComparisonGT (tvar0, tvar) -> s2s "PtnCompGT" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnComparisonLTE (tvar0, tvar) -> s2s "PtnCompLTE" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnComparisonGTE (tvar0, tvar) -> s2s "PtnCompGTE" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnReflection (tvar0) -> s2s "PtnReflection" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> c2s ')'
    |    Terms.PtnAcquisition  -> s2s "PtnAcquisition" 
    |    Terms.PtnSuspension (tvar0, tvar) -> s2s "PtnSuspension" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnRelease (tvar0, tvar) -> s2s "PtnRelease" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >> show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnInnerSuspension (tvar0, tvar) -> s2s "PtnInnerSuspend" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnDivision (tvar0, tvar) -> s2s "PtnDivision" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >> show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnAddition (tvar0, tvar) -> s2s "PtnAddition" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >> show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnMultiplication (tvar0, tvar) -> s2s "PtnMultiply" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnJuxtaposition (tvar0, tvar) -> s2s "PtnJuxtapose" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> s2s ", " >>  show_ident tvar e p k mk q >> c2s ')'
    |    Terms.PtnNegation (tvar0) -> s2s "PtnNegate" >> c2s ' ' >> c2s '(' >> show_ident tvar0 e p k mk q  >> c2s ')'


  and show_variation v e p k mk q : showable =
    match v with
        Terms.Identifier ident ->
          s2s "Identifier" >> c2s ' ' >> c2s '(' >> show_ident ident e p k mk q >> c2s ')'
      | Terms.Abandon( Terms.Wild( wild ) ) -> s2s "Abandon" >> c2s ' ' >> c2s '(' >> showString wild >> c2s ')'


  and show_lyst l e p k mk q : showable =
    match l with
      Terms.Empty  -> s2s "Empty" 
    |    Terms.Enum patterns -> s2s "Enum" >> c2s ' ' >> c2s '(' >> showList ( fun nptn -> show_pattern nptn e p k mk q ) patterns >> c2s ')'
    |    Terms.Cons (patterns, lyst) -> s2s "Cons" >> c2s ' ' >> c2s '(' >> showList ( fun nptn -> show_pattern nptn e p k mk q ) patterns  >> s2s ", " >>  show_lyst lyst e p k mk q >> c2s ')'
    |    Terms.ConsV (patterns, variation) -> s2s "ConsV" >> c2s ' ' >> c2s '(' >> showList ( fun nptn -> show_pattern nptn e p k mk q ) patterns  >> s2s ", " >>  show_variation variation e p k mk q >> c2s ')'


  and show_literal lit e p k mk q : showable =
    match lit with
        Terms.BooleanLiteral duality -> s2s "BooleanLiteral" >> c2s ' ' >> c2s '(' >> show_duality duality e p k mk q >> c2s ')'
    |    Terms.StringLiteral str -> s2s "StringLiteral" >> c2s ' ' >> c2s '(' >> showString str >> c2s ')'
    |    Terms.IntegerLiteral n -> s2s "IntegerLiteral" >> c2s ' ' >> c2s '(' >> showInt n >> c2s ')'
    |    Terms.DoubleLiteral d -> s2s "DoubleLiteral" >> c2s ' ' >> c2s '(' >> showFloat d >> c2s ')'
    |    Terms.Reification term -> s2s "Reification" >> c2s ' ' >> c2s '(' >> show_term term e p k mk q >> c2s ')'


  and show_duality d e p k mk q : showable =
    match d with
      Terms.Verity  -> s2s "Verity" 
    |    Terms.Absurdity  -> s2s "Absurdity" 


  and show_symbol s : showable =
    match s with
        Terms.Tag( Terms.LIdent( lident ) ) -> s2s "Tag" >> c2s ' ' >> c2s '(' >> showString lident >> c2s ')'

  and show_ident ident e p k mk q = 
    match ident with
        Transcription( t ) ->
          s2s "<<" >> c2s ' ' >> s2s "..." >> c2s ' ' >> s2s ">>"
      | Symbol( s ) ->
          match s with
              Symbols.URL( url ) -> showString url
            | Symbols.Opaque( opaque ) -> showString opaque
            | Symbols.Debruijn( ( i, j ) ) ->
                c2s '(' >> c2s ' ' >> showInt i >> s2s " , " >> showInt j >> c2s ')'

  let show_hypothesis t e p k mk q =
    let regsShow =
      show_env e e p k mk q >> s2s " , " >> show_prompt p e p k mk q >> s2s " , " >> show_k k e p k mk q >> s2s " , " >> show_mk mk q p k mk q  >> s2s " , " >> show_prompt q e p k mk q in
      s2s "reduce" >> c2s '(' >> show_term t e p k mk q >> s2s " , " >> regsShow >> c2s ')' 

  let show_consequent t e p k mk q = 
    match t with
        Terms.Sequence( [] ) -> s2s "()"
      | Terms.Sequence( thd :: ttl ) ->
          let seqHdShow =
            s2s "reduce" >> c2s '(' >> show_term thd e p k mk q >> c2s ')' in
          let seqTlShow =
            s2s "reduce" >> c2s '(' >> ( show_term ( Terms.Sequence ttl ) e p k mk q ) >> c2s ')' in
            seqHdShow >> s2s " ; " >> seqTlShow

      (* application *)
      | Terms.Application( op, [] ) ->
          s2s "reduce" >> c2s '(' >> show_term op e p k mk q >> c2s ')' >> s2s "()"

      | Terms.Application( op, actls ) ->           
          s2s "reduce" >> c2s '(' >> show_term op e p k mk q >> c2s ')'

      (* let *)
      | Terms.Supposition( ptn, pterm, eterm ) ->
          raise ( NotYetImplemented "show_consequent: Supposition" )

      (* letrec *)
      | Terms.Recurrence( ptn, pterm, eterm ) ->
          raise ( NotYetImplemented "show_consequent: Recurrence" )

      (* abstraction *)
      | Terms.Abstraction( ptn, eterm ) ->
          raise ( NotYetImplemented "show_consequent: Abstraction" )
            
      (* condition *)            
      | Terms.Condition( test, tbranch, fbranch ) ->
          raise ( NotYetImplemented "show_consequent: Condition" )

      (* monadic desugaring *)
      (*  This has been moved to the syntactic transform stage *)

      (* comparison *)
      | Terms.Equation( lhs, rhs ) ->
          raise ( NotYetImplemented "show_consequent: Equation" )
          
      | Terms.ComparisonLT( lhs, rhs ) ->
          raise ( NotYetImplemented "show_consequent: ComparisonLT" )

      | Terms.ComparisonGT( lhs, rhs ) ->
          raise ( NotYetImplemented "show_consequent: ComparisonGT" )

      | Terms.ComparisonLTE( lhs, rhs ) ->
          raise ( NotYetImplemented "show_consequent: ComparisonLTE" )

      | Terms.ComparisonGTE( lhs, rhs ) ->
          raise ( NotYetImplemented "show_consequent: ComparisonGTE" )

      (* reflection -- dual to reification *)
      | Terms.Reflection( v ) ->
          raise ( NotYetImplemented "show_consequent: Reflection" )

      (* delimited continuations *)
      | Terms.Acquisition -> 
          raise ( NotYetImplemented "show_consequent: Acquisition" )

      | Terms.Suspension( pterm, eterm ) -> 
          raise ( NotYetImplemented "show_consequent: Suspension" )
            
      | Terms.Release( pterm, eterm ) -> 
          raise ( NotYetImplemented "show_consequent: Release" )

      | Terms.InnerSuspension( pterm, eterm ) -> 
          raise ( NotYetImplemented "show_consequent: InnerSuspension" )

      (* primitive arithmetic calculation *)
      | Terms.Calculation( aterm ) -> 
          raise ( NotYetImplemented "show_consequent: Calculation" )            

  let show_transition t e p k mk q =
    ( show_hypothesis t e p k mk q ) >> s2s "====>" >> ( show_consequent t e p k mk q )
end
