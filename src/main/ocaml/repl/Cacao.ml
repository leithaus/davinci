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

let load_config_and_begin_repl () = 
  begin
    let config_file_name =
      ( match ( Array.length Sys.argv ) with
          2 -> Sys.argv.(1)
        | _ -> "conf.ml" ) in 

      ( CacaoScriptConfig.load_config_file config_file_name ); 

      let bctl = CacaoScriptConfig.begin_cacao_top_level() in
      let ftr = CacaoScriptConfig.file_to_read() in      
        match bctl with
            true ->
              begin
                CacaoScriptREPL.read_eval_print_loop ();                
              end
          | _ -> ()
  end;;
   
match !Sys.interactive with
    true ->
      begin      
        ( load_config_and_begin_repl () );
        let ec = CacaoScriptConfig.exit_completely() in
          match ec with
              true -> exit( 0 )
            | _ ->
                begin
                  print_string "Dropping to ocaml top level.";
                  print_newline ();
                  flush stdout;
                end
      end
  | _ -> () ;;
    
let main () = 
  ( load_config_and_begin_repl () )
;;
