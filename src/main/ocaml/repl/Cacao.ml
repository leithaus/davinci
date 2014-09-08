(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    main.ml                                                     *)
(* Authors:     lgm                                                         *)
(* Creation:    Mon Mar  7 11:42:44 2005                                    *)
(* Copyright:   Biosimilarity LLC 2004 - 2014. All rights reserved.         *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

open Monad
open REPL

module CacaoScriptREPL : REPLS = REPL( Identity_Monad ) ;;

match !Sys.interactive with
    true ->
      begin
        CacaoScriptREPL.read_eval_print_loop ();
        exit 0
      end
  | _ -> () ;;
      
let main () = 
  CacaoScriptREPL.read_eval_print_loop ()
;;
