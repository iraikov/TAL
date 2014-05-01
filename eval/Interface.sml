
(* 
 * Interface.sml
 *
 * A simple interpreter for System F -- signature definitions.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

signature VALUE =
sig
    type symbol = Symbol.symbol

    type const     = SF.const
    type typ       = SF.typ
    type exp       = SF.exp
    type program   = SF.program

    datatype oper  = Add  | Sub  | Mul  | Div    | Eq  | Less  | Or | Not
		   | IAdd' of int | ISub' of int | IMul' of int | IDiv' of int
		   | IEq' of int | ILess' of int 
         	   | RAdd' of real | RSub' of real | RMul' of real | RDiv' of real
	   	   | REq' of real | RLess' of real 
		   | Math of string
		   | Or' of bool
		   | Cons | Car | Cdr 
		   | ICons of int | RCons of real | BCons of bool | PCons of value * value
		   | PEq of value
		   | RDCons of symbol list * value Env.env 

	 and value = UnitVal
		   | NilVal      
		   | BoolVal     of bool
		   | IntNum      of int
		   | RealNum     of real
		   | PairVal     of value * value
		   | RecordVal   of symbol list * value Env.env 
		   | VectorVal   of value Vector.vector
		   | Op          of oper
		   | Proc        of symbol * exp * value Env.env
		   | RecProc     of symbol * symbol * exp * value Env.env
			    
    exception ValueError

    val constant  :  const -> value
    val applyOp   :  oper * value -> value
    val valbase   :  value Env.env
end



signature EVAL =
sig
    include VALUE

    val eval     : value Env.env -> exp -> value
    val evalProgram  : value Env.env -> program -> value
end


signature MAIN =
sig
    include EVAL

    val evalFromString   : string -> (value * typ)
    val evalFromFile     : string -> (value * typ)
    val interactiveEval  : unit -> unit
    val run              : string * string list -> unit
end


signature INTERFACE =
sig
    include MAIN
end
