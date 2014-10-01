(* -*- mode: Tuareg;-*-  *)
(* Filename:    Observation.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Sun Sep 28 13:14:09 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

module type STRINGTHEORY =
sig
  module type StringKey =
  sig
    type t = string
    val equal : 'a -> 'a -> bool
    val hash : 'a -> int
  end
  
  module StrKey : StringKey

  module type StringTable = module type of Hashtbl.Make( StrKey )
  
  module StrTbl : StringTable

  type obs_ctxt = bool StrTbl.t

  val observation_context : unit -> obs_ctxt
  val report_reductions_p : 'a StrTbl.t -> bool
  val report_reductions : 'a StrTbl.t -> 'b -> bool
end

module OBSERVER : STRINGTHEORY =
struct
  module type StringKey =
  sig
    type t = string
    val equal : 'a -> 'a -> bool
    val hash : 'a -> int
  end

  module StrKey : StringKey =
  struct
    type t = string
    let equal s1 s2 = ( s1 == s2 )
    let hash s = ( Hashtbl.hash  s )
  end

  module type StringTable = module type of Hashtbl.Make( StrKey )

  module StrTbl : StringTable = Hashtbl.Make( StrKey )

  type obs_ctxt = bool StrTbl.t

  let observation_context () =
    ( StrTbl.create 100 )
  let report_reductions_p obs_c =
    ( StrTbl.mem obs_c "report_reductions" )
  let report_reductions obs_c b =
    ( StrTbl.mem obs_c "report_reductions" )
end
