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

module CEK : ASTXFORMS = ASTXFORM( Identity_Monad )

module REPL =
struct
  let parse (c : in_channel) : Abscacao.expr = 
    Parcacao.pExpr Lexcacao.token (Lexing.from_channel c)

  let showTree (t : Abscacao.expr) : string = 
    "[Abstract syntax]\n\n" ^ (fun x -> Showcacao.show (Showcacao.showExpr x)) t ^ "\n\n" ^
      "[Linearized tree]\n\n" ^ Printcacao.printTree Printcacao.prtExpr t ^ "\n"

  let print_rslt v =
    raise ( NotYetImplemented "print_rslt" )

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
                let term = ( CEK.expr_to_term ast ) in
                let nrml = ( CEK.REval.reduce term CEK.REval.init_env CEK.REval.init_k ) in
                  begin
                    print_rslt nrml;
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
