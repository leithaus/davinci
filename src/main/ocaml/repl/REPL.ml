(* -*- mode: Tuareg;-*-  *)
(* Filename:    REPL.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Thu Sep  4 02:11:59 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Lexing
open Abscacao
open Exceptions
open Evaluator
open Monad
open Stage1

module type REPLS =
sig
  module Pipeline : ASTXFORMS

  val parse : in_channel -> Abscacao.expr
  val showTree : Abscacao.expr -> string
  val print_rslt : Pipeline.model_value -> string
  val eval : Pipeline.model_term -> Pipeline.model_value Pipeline.REval.monad
  val read_eval_print_loop : unit -> unit
end

module type REPLFUNCTOR =
  functor ( M : MONAD ) ->
sig
  module Pipeline : ( ASTXFORMS with type 'a REval.monad = 'a M.monad )

  val parse : in_channel -> Abscacao.expr
  val showTree : Abscacao.expr -> string
  val print_rslt : Pipeline.model_value -> string
  val eval : Pipeline.model_term -> Pipeline.model_value Pipeline.REval.monad
  val read_eval_print_loop : unit -> unit
end

module REPL : REPLFUNCTOR =
  functor ( M : MONAD ) ->
struct
  module Pipeline : ( ASTXFORMS with type 'a REval.monad = 'a M.monad )
    = ASTXFORM( M )

  let parse (c : in_channel) : Abscacao.expr = 
    Parcacao.pExpr Lexcacao.token (Lexing.from_channel c)

  let showTree (t : Abscacao.expr) : string = 
    "[Abstract syntax]\n\n" ^ (fun x -> Showcacao.show (Showcacao.showExpr x)) t ^ "\n\n" ^
      "[Linearized tree]\n\n" ^ Printcacao.printTree Printcacao.prtExpr t ^ "\n"

  let print_rslt v =
    raise ( NotYetImplemented "print_rslt" )

  let eval m_term =
    ( Pipeline.REval.reduce
        m_term
        Pipeline.REval.init_env
        Pipeline.REval.init_k )

  let read_eval_print_loop () = 
    (* let dbg = ref false in *)
    let rslt = ref true in
    let channel =
      if Array.length Sys.argv > 1 then
        open_in Sys.argv.(1)
      else
        stdin
    in
      print_string " *** Cacao Top Level version 0.01 *** \n";
      try
        (while (!rslt)
          do	  
	    print_string "> ";
	    flush stdout;
	    let ast = parse channel in
            let astStr = showTree ast in          
              begin	      
                print_string "Parsed.\n";
                print_string ( "ast = " ^ ( astStr ^ ".\n" ) );
                print_newline ();
                flush stdout;
                let term = ( Pipeline.expr_to_term ast ) in
                  begin
                    ( M.m_bind
                        ( eval term )
                        ( fun nrml ->
                          print_string ( print_rslt nrml );
                          ( M.m_unit nrml ) ) );
                    print_newline ();
                    flush stdout
                  end
              end
          done)
      with e ->
        begin
          print_string "caught exception...\n"; 
          print_string (Printexc.to_string e);
          print_string "\n exiting";
          print_newline ();
          flush stdout;
          ()
        end
end
