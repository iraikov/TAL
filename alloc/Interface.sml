
(* 
 * Interface.sml
 *
 * System F allocation signatures.
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

    datatype phi = UN | IN | VECT of int * int

    eqtype ptag
    eqtype ttag

    datatype typ = Nil
		 | Unit 
		 | Bool 
		 | Int 
		 | Real 
		 | Pair    of (typ * ttag) * ptag
		 | Vector  of (typ * ttag) * ptag
		 | Record  of (symbol * (typ * ttag) * int) list * ptag
		 | Arrow   of (symbol list) * ((typ * ttag) list) * (typ)
		 | Tvar    of symbol
		 | Exists  of symbol * (typ * ttag)

    type ttyp = typ * ttag

    type typenv = typ Env.env
    type ttagenv = ttag Env.env

    type phimap 
    type phislot = phi Vector.vector

    val phNull   : ptag
    val phEmpty  : phimap
    val phEnter  : (phislot * phimap) -> (ptag * phimap)
    val phFind   : (ptag * phimap)    -> phislot option


    val ttNull   : ttag
    val ttInt    : ttag -> int

    type tmap
    type utmap

    val ttBase   : utmap
    val ttMap    : utmap -> tmap
    val ttEmpty  : int -> utmap
    val ttEnter  : typ * utmap -> ttag * utmap
    val ttFind   : ttag * tmap -> typ option
    val ttAppi   : ((ttag * typ) -> unit) -> tmap -> unit
    val ttFoldli : ((ttag * typ * 'b) -> 'b) -> 'b -> tmap -> 'b

    val compose   : symbol list * symbol list * typenv -> typenv
    val unify     : ((typ) * (typ)) -> bool
    val tsubst    : typenv -> typ -> typ
				   
    val alloc_pptyp' : (typ) -> Pretty.doc
    val alloc_pptyp  : (typ) -> unit

end

signature SYNTAX =
sig

    include TYPE


    type label = symbol

    datatype refmode = READ | WRITE


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
		    

    datatype declaration =   Assign   of var     * tvalue
			   | Unpack   of symbol  * var * tvalue
			   | Memcpy   of heapvar * var * tvalue * tvalue 
			   | Lmalloc  of label   * (typ * ttag)
			   | Rmalloc  of label   * (symbol * (typ * ttag) * int) list
			   | Vmalloc  of label   * (typ * ttag) * int

  
    datatype term =    App     of tvalue * tvalue list
		     | If0     of tvalue * term * term     
		     | Halt    of (typ) * tvalue
		     | Let     of declaration list * term

    type block    = symbol list * ((symbol * (typ * ttag)) list) * term

    type heap     = (heapvar * (typ) * (block option)) Env.env
		   
    type program  = symbol * (typ) * tmap * heap * phimap * term

end

signature PRETTYPRINT = 
sig
    include SYNTAX

    val alloc_ppval'   : value  -> Pretty.doc
    val alloc_ppblock' : label * block  -> Pretty.doc
    val alloc_ppdecl'  : declaration   -> Pretty.doc
    val alloc_pp'      : term   -> Pretty.doc

    val alloc_ppval    : value  -> unit
    val alloc_ppblock  : label * block  -> unit
    val alloc_ppdecl   : declaration  -> unit
    val alloc_pp       : term   -> unit

    val alloc_ppblockWithStream : TextIO.outstream -> (label * block) -> unit
    val alloc_ppWithStream : TextIO.outstream -> term -> unit
end

signature SEMANT = 
sig
    include PRETTYPRINT

    val alloc_Dt : heap    -> phimap -> typenv  -> declaration list -> (phimap * typenv)
    val alloc_Vt : heap    -> phimap -> typenv  -> value   -> (typ)
    val alloc_Et : heap    -> phimap -> typenv  -> term    -> unit
    val alloc_Bt : heap    -> phimap -> typenv  -> block   -> unit
    val alloc_Pt : typenv  -> program -> unit
end

signature TRANSFORM =
sig
    include SEMANT

    val alloc_transform_type      : (Clos.typ * (utmap * phimap)) -> (typ * (utmap * phimap))
    val alloc_transform_exp       : (Clos.exp * (utmap * heap * phimap * typenv * ttagenv)) -> 
				    (term * (utmap * heap * phimap))
    val alloc_transform_program   : Clos.program -> program

    val alloc_typebase : {symbols: symbol list, env: typenv, phimap: phimap, utmap: utmap}
end

signature INTERFACE = 
sig
    include TRANSFORM
end
 
