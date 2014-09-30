(* -*- mode: Tuareg;-*-  *)
(* Filename:    Observation.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Sun Sep 28 13:14:09 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

module type StringKey =
sig
  type t = string
  val equal : string -> string -> bool
  val hash : string -> int
end

module type StringTable =
sig
  type key = StringKey.t
  type 'a t = 'a BatHashtbl.Make(StringKey).t
  val create : int -> 'a t
  val length : 'a t -> int
  val is_empty : 'a t -> bool
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val remove_all : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_all : 'a t -> key -> 'a list
  val find_default : 'a t -> key -> 'a -> 'a
  val find_option : 'a t -> key -> 'a option
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map : (key -> 'b -> 'c) -> 'b t -> 'c t
  val map_inplace : (key -> 'a -> 'a) -> 'a t -> unit
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_inplace : ('a -> bool) -> 'a t -> unit
  val filteri : (key -> 'a -> bool) -> 'a t -> 'a t
  val filteri_inplace : (key -> 'a -> bool) -> 'a t -> unit
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
  val modify : key -> ('a -> 'a) -> 'a t -> unit
  val modify_def : 'a -> key -> ('a -> 'a) -> 'a t -> unit
  val modify_opt : key -> ('a option -> 'a option) -> 'a t -> unit
  val keys : 'a t -> key BatEnum.t
  val values : 'a t -> 'a BatEnum.t
  val enum : 'a t -> (key * 'a) BatEnum.t
  val of_enum : (key * 'a) BatEnum.t -> 'a t
  val print :
    ?first:string ->
    ?last:string ->
    ?sep:string ->
    ('a BatInnerIO.output -> key -> unit) ->
    ('a BatInnerIO.output -> 'b -> unit) ->
    'a BatInnerIO.output -> 'b t -> unit
  module Exceptionless :
  sig
    val find : 'a t -> key -> 'a option
    val modify :
      key -> ('a -> 'a) -> 'a t -> (unit, exn) BatPervasives.result
  end
  module Infix :
  sig
    val ( --> ) : 'a t -> key -> 'a
    val ( <-- ) : 'a t -> key * 'a -> unit
  end
  module Labels :
  sig
    val add : 'a t -> key:key -> data:'a -> unit
    val replace : 'a t -> key:key -> data:'a -> unit
    val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
    val map : f:(key:key -> data:'a -> 'b) -> 'a t -> 'b t
    val map_inplace : f:(key:key -> data:'a -> 'a) -> 'a t -> unit
    val filter : f:('a -> bool) -> 'a t -> 'a t
    val filter_inplace : f:('a -> bool) -> 'a t -> unit
    val filteri : f:(key:key -> data:'a -> bool) -> 'a t -> 'a t
    val filteri_inplace : f:(key:key -> data:'a -> bool) -> 'a t -> unit
    val filter_map : f:(key:key -> data:'a -> 'b option) -> 'a t -> 'b t
    val filter_map_inplace :
      f:(key:key -> data:'a -> 'a option) -> 'a t -> unit
    val fold :
      f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
    val modify : key:key -> f:('a -> 'a) -> 'a t -> unit
    val modify_def :
      default:'a -> key:key -> f:('a -> 'a) -> 'a t -> unit
    val modify_opt :
      key:key -> f:('a option -> 'a option) -> 'a t -> unit
  end
end

module type OBSERVE =
sig
  module StrKey : StringKey
  module StrTbl : StringTable

  type obs_ctxt = bool StringTable.t

  val report_reductions_p : obs_ctxt -> bool
  val report_reductions : obs_ctxt -> bool -> unit
end


module OBSERVER : OBSERVE =
struct
  module StrKey : StringKey =
  struct
    type t = string
    let equal s1 s2 = ( s1 == s2 )
    let hash s = ( Hashtbl.hash  s )
  end

  module StrTbl : StringTable = Hashtbl.Make( StringKey )

  type obs_ctxt = bool StringTable.t
  let report_reductions_p obs_c =
    ( StringTable.mem obs_c "report_reductions" )
  let report_reductions obs_c b =
    ( StringTable.mem obs_c "report_reductions" b )
end
