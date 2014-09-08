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

  (* We create a cp foo of type int. Its default value is 42: *)
  let show_parse_tree_param =
    new bool_cp ~group ["section1";"show_parse_tree"] false "Toggle whether to show parse tree."
    
  let show_parse_tree () = show_parse_tree_param#get
    (* We read cacaoscript toplevel config from file "conf.ml" *)
  let load_config_file fname = group#read fname    
end
