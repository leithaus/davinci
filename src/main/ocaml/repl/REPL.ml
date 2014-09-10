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
open Cfg

module type REPLS =
sig
  module Pipeline : ASTXFORMS

  val parse : in_channel -> Abscacao.request
  val showTree : Abscacao.request -> string
  val print_rslt : Pipeline.model_value -> string
  val eval : Pipeline.model_term -> Pipeline.model_value Pipeline.REval.monad
  val read_eval_print_loop : unit -> unit
end

module type REPLFUNCTOR =
  functor ( M : MONAD ) ->
sig
  module Pipeline : ( ASTXFORMS with type 'a REval.monad = 'a M.monad )

  val parse : in_channel -> Abscacao.request
  val showTree : Abscacao.request -> string
  val print_rslt : Pipeline.model_value -> string
  val eval : Pipeline.model_term -> Pipeline.model_value Pipeline.REval.monad
  val read_eval_print_loop : unit -> unit
end

module REPL : REPLFUNCTOR =
  functor ( M : MONAD ) ->
struct
  module Pipeline : ( ASTXFORMS with type 'a REval.monad = 'a M.monad )
    = ASTXFORM( M )

  let parse (c : in_channel) : Abscacao.request = 
    Parcacao.pRequest Lexcacao.token (Lexing.from_channel c)

  let showTree (t : Abscacao.request) : string = 
    "[Abstract syntax]\n\n" ^ (fun x -> Showcacao.show (Showcacao.showRequest x)) t ^ "\n\n" ^
      "[Linearized tree]\n\n" ^ Printcacao.printTree Printcacao.prtRequest t ^ "\n"

  let print_rslt v =
    raise ( NotYetImplemented "print_rslt" )

  let eval m_term =
    ( Pipeline.REval.reduce
        m_term
        Pipeline.REval.init_env (* BUGBUG -- lgm -- this assumes no builtin fns *)
        Pipeline.REval.init_k )

  let evaluate_expression ast =
    let desugared_ast = ( Pipeline.desugar ast ) in
    let term = ( Pipeline.expr_to_term desugared_ast ) in
      ( eval term )

  let report v =
    begin
      ( M.m_bind
          v
          ( fun nrml ->
            print_string ( print_rslt nrml );
            ( M.m_unit nrml ) ) );
      print_newline ();
      flush stdout
    end

  let read_eval_print_loop () = 
    (* let dbg = ref false in *)    
    let rslt = ref true in
    let show_tree = 
      CacaoScriptConfig.show_parse_tree() in
    let channel =
      ( match CacaoScriptConfig.file_to_read() with
          Some( file_name ) -> ( open_in file_name )
        | None -> stdin )
    in
      print_string " *** Cacao Top Level version 0.01 *** \n";
      ( print_string 
        (
          " show parse tree turned: " 
          ^ ( match show_tree with 
              true -> "on"
            | false -> "off" )
          ^ "\n"
        ) );

      try
        (while (!rslt)
          do	  
	    print_string "> ";
	    flush stdout;
	    let ast = parse channel in            
              begin	                      
                print_string "Parsed.\n";
                ( match show_tree with
                    true ->
                      let astStr = showTree ast in          
                        print_string ( "ast = " ^ ( astStr ^ ".\n" ) )
                  | _ -> () );
                print_newline ();
                flush stdout;
                ( match ast with 
                    Evaluation( expr_ast ) ->
                      let v = ( evaluate_expression expr_ast ) in
                        ( report v )
                  | TypeCheck( expr_ast, type_ast ) ->
                      raise ( NotYetImplemented "TypeCheck" )
                  | ModelCheck( expr_ast, form_ast ) ->
                      raise ( NotYetImplemented "ModelCheck" )
                  | OuterShell( osreq ) ->
                      raise ( NotYetImplemented "OuterShell" )
                  | InnerShell( isreq ) ->
                      match isreq with
                          ExitRequest -> rslt := true
                        | _ -> raise ( NotYetImplemented "other inner shell requests" ) )
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
