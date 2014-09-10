(* -*- mode: Tuareg;-*-  *)
(* Filename:    Cfg.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Mon Sep  8 02:05:59 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

open Config_file

module CacaoScriptConfig =
struct
  let group = new group

  (* We create a cp show_parse_tree of type bool. Its default value is false *)
  let show_parse_tree_param =
    new bool_cp ~group ["section1";"show_parse_tree"] false "Toggle whether to show parse tree."
    
  let show_parse_tree () = show_parse_tree_param#get

  (* We create a cp begin_cacao_top_level of type bool. Its default value is true *)
  let begin_cacao_top_level_param =
    new bool_cp ~group ["section1";"begin_cacao_top_level"] true "Toggle whether to begin top level."
    
  let begin_cacao_top_level () = begin_cacao_top_level_param#get

    (* We read cacaoscript toplevel config from file "conf.ml" *)
  let load_config_file fname = group#read fname    
end
