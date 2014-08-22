(* -*- mode: Tuareg;-*-  *)
(* Filename:    Monad.ml  *)
(* Authors:     lgm                                                     *)
(* Creation:    Fri Aug 22 00:31:19 2014  *)
(* Copyright:   Not supplied  *)
(* Description:  *)
(* ------------------------------------------------------------------------ *)

module type MONAD =
sig
  type 'a monad
  val m_unit : 'a -> 'a monad
  val m_bind : 'a monad -> ( 'a -> 'b monad ) -> 'b monad
end

module Identity_Monad : MONAD
= struct
  type 'a monad = 'a
  let m_unit a = a
  let m_bind m k = ( k m )
end
