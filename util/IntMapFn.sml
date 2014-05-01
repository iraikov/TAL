
(* 
 * IntMapFn.sml
 *
 * A polymorphic integer map.
 *
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


signature INT_MAP = 
sig
   eqtype key

   type 'a map

   val empty       : 'a map
   val enter       : 'a map * key * 'a -> 'a map
   val find        : 'a map * key -> 'a option
   val remove      : 'a map * key -> 'a map 
   val numItems    : 'a map -> int
   val listKeys    : 'a map -> int list
   val listItems   : 'a map -> 'a list
   val foldl       : (('a * 'b) -> 'b) -> 'b -> 'a map -> 'b 
   val foldli      : ((int * 'a * 'b) -> 'b) -> 'b -> 'a map -> 'b 
   val map         : ('a -> 'b) -> 'a map -> 'b map
   val mapi        : ((int * 'a) -> 'b) -> 'a map -> 'b map
   val mapPartial  : ('a -> 'b option) -> 'a map -> 'b map
   val app         : ('a -> unit) -> 'a map -> unit
   val appi        : ((int * 'a) -> unit) -> 'a map -> unit
   val equal       : ('a * 'a -> bool) -> ('a map * 'a map) -> bool
end

functor IntMapFn (eqtype key
                  val getInt: key -> int) : INT_MAP =

struct
  type key = key

  structure H = RedBlackMapFn (struct
			       type ord_key = int
			       val compare = Int.compare
			       end)
					    
  type 'a map = 'a H.map
  val empty = H.empty

  exception NotFound

  fun find(t, k)          = H.find(t, getInt k)
  fun enter (t, k, a)     = H.insert(t, getInt k, a)
  fun remove(t, k)        = #1 (H.remove(t, getInt k))
  fun numItems(t)         = H.numItems (t)
  fun listKeys (t)        = map (fn(k,el) => k) (H.listItemsi t)
  fun listItems (t)       = H.listItems (t)
  fun mapi(f)             = H.mapi (f)
  fun map(f)              = H.map (f)
  fun mapPartial(f)       = H.mapPartial (f)
  fun app(f)              = H.app (f)
  fun appi(f)             = H.appi (f)
  fun foldl(f)            = H.foldl (f)
  fun foldli(f)           = H.foldli (f)
  fun equal f (m,n)       = (if H.numItems(m) = H.numItems(n) 
			     then (let
				       val items = H.listItemsi m
				       fun compare ((i,a)::m) = (case H.find (n,i)  of 
								     SOME b => if f(a,b) then compare m else false
								   | NONE   => false)
					 | compare nil = true
				   in
				       compare items
				   end)
			     else false)
			    
end
