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
open Cfg

module CacaoScriptREPL : REPLS = REPL( Identity_Monad ) ;;

( CacaoScriptConfig.load_config_file "conf.ml" );;

match !Sys.interactive with
    true ->
      match CacaoScriptConfig.begin_cacao_top_level() with
          true ->
            begin
              CacaoScriptREPL.read_eval_print_loop ();
              exit 0
            end
        | _ -> ()
  | _ -> () ;;
      
let main () = 
  match CacaoScriptConfig.begin_cacao_top_level() with
      true ->
        begin
          CacaoScriptREPL.read_eval_print_loop ();
        end
    | _ -> ()
;;
