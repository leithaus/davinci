(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    main.ml                                                     *)
(* Authors:     lgm                                                         *)
(* Creation:    Mon Mar  7 11:42:44 2005                                    *)
(* Copyright:   Biosimilarity LLC 2004 - 2014. All rights reserved.         *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

open Lexing

let parse (c : in_channel) : Abscacao.expr = 
    Parcacao.pExpr Lexcacao.token (Lexing.from_channel c)
;;

let showTree (t : Abscacao.expr) : string = 
    "[Abstract syntax]\n\n" ^ (fun x -> Showcacao.show (Showcacao.showExpr x)) t ^ "\n\n" ^
    "[Linearized tree]\n\n" ^ Printcacao.printTree Printcacao.prtExpr t ^ "\n"
;;

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
      end;;

if !Sys.interactive
then ()
else begin
    read_eval_print_loop ();
    exit 0
  end;;

let main () = 
  read_eval_print_loop ()
;;
