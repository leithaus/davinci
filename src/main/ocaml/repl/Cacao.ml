(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    main.ml                                                     *)
(* Authors:     lgm                                                         *)
(* Creation:    Mon Mar  7 11:42:44 2005                                    *)
(* Copyright:   Biosimilarity LLC 2004 - 2014. All rights reserved.         *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

open REPL

let dummy () =
  begin
    print_string "here we go...";
    print_newline;
    flush stdout;
  end;;

match !Sys.interactive with
    true ->
      begin
        REPL.read_eval_print_loop ();
        exit 0
      end
  | _ -> () ;;
      
let main () = 
  REPL.read_eval_print_loop ()
;;
