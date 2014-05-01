
(* 
 * Symbol.sml
 *
 * Symbol generation and manipulation; polymorphic symbol maps.
 *
 * Version $Revision: 1.1.1.1 $
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

signature SYMBOL =
sig
    type symbol = string * int

    val equal       : symbol * symbol -> bool
    val compare     : symbol * symbol -> order
    val reset       : unit -> unit
    val fresh       : unit -> symbol
    val freshPrefix : string -> symbol
    val symbol      : string -> symbol
    val toInt       : symbol -> int
    val name        : symbol -> string
end


structure Symbol :> SYMBOL =
struct
  exception Symbol

  type symbol = string * int

  structure H = HashTable

  val nextsym = ref 1
  val sizeHint = 1024
  val hashtable : (string,int) H.hash_table = 
		H.mkTable(HashString.hashString, op = ) (sizeHint,Symbol)
  
  fun symbol name =
      case H.find hashtable name
       of SOME i => (name,i)
        | NONE => let val i = !nextsym
	           in nextsym := i+1;
		      H.insert hashtable (name,i);
		      (name,i)
		  end


  val nextid = ref 0
	       

  fun freshPrefix (prefix) = (symbol (prefix ^ "_" ^ (Int.toString (!nextid))) 
		  before nextid := (!nextid) + 1)

  fun fresh () = freshPrefix "id"
		 
  fun compare ((_,s1),(_,s2)) = Int.compare (s1,s2)
  fun equal ((_,s1),(_,s2))   = s1=s2

  fun toInt (s,n) = n
  fun name (s,n) = s

  fun reset () = (nextid := 0)
		   
end

(*
 * $Id: Symbol.sml,v 1.1.1.1 2006/06/19 19:40:34 ivan_raikov Exp $
 *
 * 
 * $Log: Symbol.sml,v $
 * Revision 1.1.1.1  2006/06/19 19:40:34  ivan_raikov
 * Imported sources
 *
 *
 *)
