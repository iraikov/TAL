
(* 
 * Interface.sml
 *
 * System F continuation-passing style signatures.
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
    val cps_pptyp' : typ -> Pretty.doc
    val cps_pptyp  : typ -> unit


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
		   | Prim       of value * typ list * value
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

    val cps_ppval' : value -> Pretty.doc
    val cps_pp'    : exp -> Pretty.doc

    val cps_ppval  : value -> unit
    val cps_pp     : exp -> unit

    val cps_ppWithStream : TextIO.outstream -> exp -> unit
end

signature SEMANT = 
sig
    include PRETTYPRINT

    val cps_Vt : typenv -> value -> typ
    val cps_Et : typenv -> exp   -> unit
    val cps_Pt : typenv -> program -> unit
end

signature TRANSFORM =
sig
    include SEMANT

    exception TransformTypeError    of SF.typ
    exception TransformExpError     of SF.exp
    exception TransformTexpError    of SF.exp
    exception TransformPexpError    of SF.exp
    exception TransformProgramError of SF.program

    val cps_transform_type    : SF.typ -> typ
    val cps_transform_exp     : SF.exp -> exp
    val cps_transform_program : SF.program -> program
    val cps_typebase          : {symbols: symbol list, env: typenv}


end

signature INTERFACE = 
sig
    include TRANSFORM
end
