
(* 
 * Interface.sml
 *
 * Definition of a language based on System F.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


signature SYNTAX =
sig

    type symbol  = Symbol.symbol

		   
    datatype const   = UNIT_CONST
		     | NIL_CONST  
		     | REAL_CONST  of real
		     | INT_CONST   of int 
		     | BOOL_CONST  of bool
	       
    datatype typ = Nil
		 | Unit 
		 | Bool 
		 | Int 
		 | Real 
		 | Pair    
		 | Field   of symbol * typ
		 | Record
		 | Vector
		 | Arrow   
		 | Tvar    of symbol
		 | Tabs    of symbol list * typ 
		 | Tcon    of typ * typ list
			      

    datatype exp =
	     Const   of const               
	   | Var     of symbol              
	   | If      of exp * exp * exp     
	   | App     of exp * exp           
	   | Abs     of symbol * typ * exp 
	   | Let1    of (symbol * typ * exp) * exp
	   | Tapp    of exp * typ list       
	   | Tlet    of (symbol * typ) list * exp            
	   | Tbox    of symbol  * (symbol * typ) list * typ  
	   | Tlam    of (symbol list) * exp
	   | Rcon    of (symbol * exp) list  
	   | Rsel    of exp * symbol  
	   | Vcon    of exp list
	   | Vref    of exp * exp
	   | Texp    of typ * exp
	   | Tprim   of typ * exp

			
    type program = symbol * (symbol * typ * exp) list * typ * exp
		   
    datatype toplevel = Exp of exp
		      | Program of program
end


signature TYPE =
sig
    include SYNTAX


    type typenv = typ Env.env

    val pptyp'    : typ -> Pretty.doc
    val pptyp     : typ -> unit
    val compose   : symbol list * symbol list * typenv -> typenv
    val tsubst    : typenv -> typ -> typ
    val constant  : const -> typ
    val typebase  : {symbols: symbol list, env: typenv}
end


signature SUGAR =
sig
    include TYPE

    datatype sugarexp = Const'  of const
		      | Var'    of symbol
		      | If'     of sugarexp * sugarexp * sugarexp
		      | App'    of sugarexp * sugarexp
		      | Abs'    of symbol   * typ * sugarexp
		      | Tlam'   of symbol list * sugarexp
		      | Tapp'   of sugarexp * typ list
		      | Tlet'   of (symbol  * typ) list * sugarexp
		      | Rcon'   of (symbol  * sugarexp) list
		      | Rsel'   of sugarexp * symbol
		      | Vcon'   of sugarexp list
		      | Vref'   of sugarexp * sugarexp

		      | Cond    of (sugarexp * sugarexp) list * sugarexp
		      | Lambda  of (symbol * typ) list * sugarexp
		      | Let     of (symbol * typ * sugarexp) list * sugarexp

    type program' = symbol * (symbol * typ * sugarexp) list * typ * sugarexp
		    
    datatype toplevel' = Exp' of sugarexp
		       | Program' of program'

    val transform: sugarexp -> exp

    val transform_program: program' -> program
end

signature PRETTYPRINT = 
sig
    include SUGAR

    val pp' : toplevel -> Pretty.doc
    val pp  : toplevel -> unit
    val ppexp  : exp -> unit
    val ppWithStream  : TextIO.outstream -> toplevel -> unit
end

signature ELAB =
sig
    include PRETTYPRINT

    exception ElabError
    val unify   : typ * typ -> bool
    val esubst  : typenv -> exp -> exp
    val elab    : typ Env.env -> exp -> exp
    val elab_program : typ Env.env -> program ->  typ * program
end

signature PARSER =
sig
    include ELAB

    type pos = int
    type name = {name: symbol, pos: pos}

    val parse : ({pbool:  Sexp.sexp -> bool,
		  pname:  Sexp.sexp -> name,
		  pint:   Sexp.sexp -> int,
		  preal:  Sexp.sexp -> real}) -> Sexp.sexp -> toplevel'

    val unparse : ({ubool:  bool -> Sexp.sexp,
		    uname:  name -> Sexp.sexp,
		    uint:   int  -> Sexp.sexp,
		    ureal:  real -> Sexp.sexp})  -> toplevel' -> Sexp.sexp



    val toSexp    : toplevel' -> Sexp.sexp 
    val toSXML    : toplevel' -> Sexp.sexp  
    val fromSexp  : Sexp.sexp -> toplevel' 
    val fromSXML  : Sexp.sexp -> toplevel'  
end


signature INTERFACE =
sig
    include PARSER
end
