
(* 
 * Syntax.sml
 *
 * System F allocation syntax.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Syntax :> SYNTAX =
struct

open Type

type label = symbol

datatype refmode = READ | WRITE

type ttyp = typ * ttag

datatype value = NilVal
	       | UnitVal
	       | Var        of symbol
	       | BoolVal    of bool
	       | IntNum     of int
	       | RealNum    of real
	       | Pack       of (typ * ttag) * (value * ttag) * (typ * ttag)
	       | Tapp       of value * typ
	       | Prim       of value * ttyp list * value list
	       | Label      of label
	       | Ref        of refmode * (value * ttag) * (value * ttag)
	       | Field      of symbol * int

type tvalue   = value  * ttag
type var      = symbol * ttag
type heapvar  = label  * ttag


datatype declaration =   Assign    of var     * tvalue
		       | Unpack    of symbol  * var * tvalue
		       | Memcpy    of heapvar * var * tvalue * tvalue 
		       | Lmalloc   of label   * ttyp
		       | Rmalloc   of label   * (symbol * ttyp * int) list
		       | Vmalloc   of label   * ttyp * int
			       
datatype term =    App      of tvalue * tvalue list
		 | If0      of tvalue * term * term     
		 | Halt     of typ * tvalue
		 | Let      of declaration list * term

type block    = symbol list * ((symbol * ttyp) list) * term

type heap     = (heapvar * typ * (block option)) Env.env
		    
type program  = symbol * typ * tmap * heap * phimap * term


end
