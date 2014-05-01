
(* 
 * Interface.sml
 *
 * System F/CPS primitive conversion signatures.
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
		 | Record  of (symbol * typ) list
		 | Vector  of typ
		 | Arrow   of (symbol list) * (typ list) * typ
		 | Tvar    of symbol

    type typenv = typ Env.env

    val compose    : symbol list * symbol list * typenv -> typenv
    val tsubst     : typenv -> typ -> typ
    val unify      : (typ * typ) -> bool
    val partial    : typ * typ list -> typ
    val prim_pptyp' : typ -> Pretty.doc
    val prim_pptyp  : typ -> unit


end

signature SYNTAX =
sig

    include TYPE
		
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
		   | Prim       of value * typ list * value list
		   | Proc       of symbol list * (symbol * typ) list * exp

	 and exp =
             App     of value  * typ list * value list
	   | Halt    of typ    * value
	   | If0     of value  * exp * exp     
	   | Let     of symbol * value * exp

    type program = symbol * typ * exp
		   
end

signature PRETTYPRINT = 
sig
    include SYNTAX

    val prim_ppval' : value -> Pretty.doc
    val prim_pp'    : exp -> Pretty.doc
    val prim_ppval  : value -> unit
    val prim_pp     : exp -> unit
    val prim_ppWithStream : TextIO.outstream -> exp -> unit
end

signature SEMANT = 
sig
    include PRETTYPRINT

    val prim_Vt : typenv -> value -> typ
    val prim_Et : typenv -> exp   -> unit
    val prim_Pt : typenv -> program -> unit
end

signature TRANSFORM =
sig
    include SEMANT

    exception TransformTypeError    of CPS.typ
    exception TransformExpError     of CPS.exp
    exception TransformValError     of CPS.value
    exception TransformProgramError of CPS.program

    val prim_transform_type    : CPS.typ -> typ
    val prim_transform_exp     : (symbol list ) -> typenv -> CPS.exp -> exp
    val prim_transform_program : CPS.program -> program
    val prim_typebase          : {symbols: symbol list, env: typenv}


end

signature INTERFACE = 
sig
    include TRANSFORM
end
