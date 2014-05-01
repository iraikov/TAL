
(* 
 * Interface.sml
 *
 * System F closure conversion signatures.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

signature TYPE  =
sig

    type symbol  = Symbol.symbol

    datatype typ = Nil
		 | Unit 
		 | Bool 
		 | Int 
		 | Real 
		 | Pair    of typ
		 | Record  of symbol list * typ Env.env
		 | Vector  of typ
		 | Arrow   of (symbol list) * (typ list) * typ
		 | Tvar    of symbol
		 | Exists  of symbol * typ

    type typenv = typ Env.env
    type typenv' = {s: symbol, t: typ} Env.env

    val compose   : symbol list * symbol list * typenv -> typenv
    val tsubst    : typenv -> typ -> typ
    val unify     : (typ * typ) -> bool
    val clos_pptyp' : typ -> Pretty.doc
    val clos_pptyp  : typ -> unit
end

signature SYNTAX =
sig

    include TYPE
    
    type label = symbol

    datatype value = NilVal
		   | UnitVal
		   | Var        of symbol
		   | BoolVal    of bool
		   | IntNum     of int
		   | RealNum    of real
		   | PairVal    of value * value
		   | RecordVal  of symbol list * value Env.env 
		   | VectorVal  of value Vector.vector
		   | Vref       of value  * value 
		   | Rsel       of value * symbol
		   | Pack       of typ * value * typ
		   | Tapp       of value * typ
		   | Prim       of value * typ list * value list
		   | Label      of label
   
				   
	 and exp =   App     of value  * value list
		   | Halt    of typ    * value
		   | If0     of value  * exp * exp     
		   | Let     of symbol * value  * exp
		   | Unpack  of symbol * symbol * value * exp
			
    type block   = label * symbol list * (symbol * typ) list * exp
		   
    type heap    = block Env.env
		   
    type program = symbol * typ * heap * exp
		   
end

signature PRETTYPRINT = 
sig
    include SYNTAX

    val clos_ppval' : value  -> Pretty.doc
    val clos_pp'    : exp    -> Pretty.doc

    val clos_ppval  : value  -> unit
    val clos_pp     : exp    -> unit

    val clos_ppWithStream     : TextIO.outstream -> exp -> unit
    val clos_ppblockWithStream     : TextIO.outstream -> block -> unit

end

signature SEMANT = 
sig
    include PRETTYPRINT

    val clos_Vt : heap -> symbol list -> typenv' -> typenv' -> value -> typ
    val clos_Et : heap -> symbol list -> typenv' -> typenv' -> exp -> unit
    val clos_Bt : heap -> typenv' -> block -> unit
    val clos_Pt : typenv' -> program  -> unit
end

signature TRANSFORM =
sig
    include SEMANT

    val clos_transform_type      : Prim.typ -> typ
    val clos_transform_exp       : heap -> (symbol list) -> typenv' -> Prim.exp -> (heap * exp)
    val clos_transform_program   : Prim.program -> program

    val clos_typebase : ({s: symbol, t: typ} Env.env)

end

signature INTERFACE = 
sig
    include TRANSFORM
end
