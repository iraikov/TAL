
(* 
 * Env.sml
 *
 * An environment for binding identifiers to arbitrary data types. 
 *
 * Version $Revision: 1.2 $
 *
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 * 02110-1301 USA
 *
 * A full copy of the GPL license can be found on Debian systems in 
 * /usr/share/common-licenses/GPL-2
 *
 *)


signature ENV =
sig
    type symbol = Symbol.symbol
    type 'a env

    exception Unbound of symbol

    val empty  : 'a env
    val enter  : symbol * 'a * 'a env -> 'a env
    val look   : symbol * 'a env -> 'a		
    val find   : symbol * 'a env -> 'a option
    val remove : symbol * 'a env -> 'a env
    val mem    : symbol * 'a env -> bool
    val foldl  : (('a * 'b) -> 'b) -> 'b -> 'a env -> 'b 
    val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> 'a env -> 'b 
    val mapi   : ((int * 'a) -> 'b) -> 'a env -> 'b env
    val map    : ('a -> 'b) -> 'a env -> 'b env
    val mapPartial : ('a -> 'b option) -> 'a env -> 'b env
    val app    : ('a -> unit) -> 'a env -> unit
    val equal  : ('a * 'a -> bool) -> 'a env * 'a env -> bool

    val numItems    : 'a env -> int
    val listKeys    : 'a env -> int list
    val listItems   : 'a env -> 'a list

end


structure Env :> ENV =
struct
    type symbol   = Symbol.symbol

    exception Unbound of symbol

    structure M = IntMapFn(type key = symbol
                           val getInt = Symbol.toInt)
    
    type 'a env   = 'a M.map
		    
    val empty               = M.empty
    fun enter (k,v,e)       = M.enter(e,k,v)
    fun look (k,e)          = (case M.find(e,k) of
				   SOME v => v
				 | NONE   => raise Unbound k 
						   before print ("unbound symbol: " ^ (Symbol.name k) ^ "\n"))
    fun find (k, e)         = M.find (e,k)
    fun mem (k,e)            = (case M.find(e,k) of
				   SOME v => true
				 | NONE   => false)
			      
    fun remove (k,e) = M.remove (e,k)

    val map        = M.map
    val mapi       = M.mapi
    val mapPartial = M.mapPartial
    val app        = M.app
    val foldl      = M.foldl
    val foldli     = M.foldli
    val equal      = M.equal
    val listKeys   = M.listKeys
    val listItems  = M.listItems
    val numItems   = M.numItems

end


(*
 * $Id: Env.sml,v 1.2 2006/07/18 21:44:12 ivan_raikov Exp $
 *
 * 
 * $Log: Env.sml,v $
 * Revision 1.2  2006/07/18 21:44:12  ivan_raikov
 * -Added dlet/detach/fetch distributed computing instructions
 * -Added simple copy propagation optimization to allocation module
 *
 * Revision 1.1.1.1  2006/06/19 19:40:34  ivan_raikov
 * Imported sources
 *
 *
 *)
