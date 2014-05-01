
(* 
 * Syntax.sml
 *
 * System F continuation-passing style syntax.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Syntax: SYNTAX =
struct

open Type


datatype value = NilVal
	       | UnitVal
	       | Var        of symbol
	       | BoolVal    of bool
	       | IntNum     of int
	       | RealNum    of real
	       | PairVal    of value * value
	       | RecordVal  of symbol list * value Env.env 
 	       | VectorVal  of value Vector.vector
	       | Vref       of value * value 
	       | Rsel       of value * symbol
	       | Prim       of value * typ list * value
	       | Proc       of symbol list * (symbol * typ) list * exp

and exp =
         App     of value  * typ list * value list
       | Halt    of typ    * value
       | If0     of value  * exp * exp     
       | Let     of symbol * value * exp
		    
type program = symbol * typ * exp


end
