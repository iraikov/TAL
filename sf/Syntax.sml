
(* 
 * Syntax.sml
 *
 * Abstract syntax definition of System F.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Syntax : SYNTAX =
struct

type symbol  = Symbol.symbol
type pos = int
type name = {name: symbol, pos: pos}

fun symofname {pos=_, name=sym} = sym fun posofname {pos=pos, name=_}
                                          = pos fun makename (n,p) = {name=n, pos=p}

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
	 Const    of const               
         | Var      of symbol              
         | If       of exp * exp * exp     
         | App      of exp * exp           
         | Abs      of symbol * typ * exp 
         | Let1     of (symbol * typ * exp) * exp
         | Tapp     of exp * typ list       
         | Tlet     of (symbol * typ) list * exp            
         | Tbox     of symbol  * (symbol * typ) list * typ  
         | Tlam     of (symbol list) * exp
         | Rcon     of (symbol * exp) list  
         | Rsel     of exp * symbol  
         | Vcon     of exp list
         | Vref     of exp * exp
         | Texp     of typ * exp
         | Tprim    of typ * exp

		                 
type program = symbol * (symbol * typ * exp) list * typ * exp

datatype toplevel = Exp of exp
		  | Program of program


fun constTyp n = case n of UNIT_CONST       => Unit
			 | NIL_CONST        => Nil
			 | BOOL_CONST _     => Bool
			 | INT_CONST  _     => Int
			 | REAL_CONST _     => Real
					           
					           
fun compose (a,v,T1) = 
    let 
	fun compose1 (a,v,T1) =
	    let 
		val a' = Env.find (a,T1)
	    in
		case a' of 
		    SOME a' => 
		    let
			val T2  = Env.enter (a,Tvar v,Env.empty)
			val T1' = Env.map (fn(t) => tsubst T2 t) T1
			val T1' = Env.enter (a, Tvar v, T1')
		    in
			Env.enter (v,a',T1')
		    end
		  | NONE    => Env.enter (a,Tvar v,T1)
	    end
    in
	ListPair.foldl compose1 T1 (a,v)
    end
        
and  tsubst  T (Tvar a)            = (Env.look (a, T) handle Env.Unbound _ => (Tvar a))
   | tsubst  T (Tcon (u, v))       = Tcon (tsubst T u, map (fn(v) => tsubst  T v) v)
   | tsubst  T (Tabs (a, u))       =
     let
	 val v  = List.tabulate (length a, fn(x) => Symbol.freshPrefix "sf")
     in
	 Tabs (v, tsubst (compose (a,v,T)) u)
     end
   | tsubst T (Field (x,t))       = Field(x, tsubst T t)
   | tsubst T t                   =  t
                                         
                                         
val mathTyp  = Tcon (Arrow, [Real, Real])

val arithTyp = let val a = Symbol.symbol "a"
	       in
		   Tabs ([a],  Tcon (Arrow, [Tvar a, Tcon (Arrow, [Tvar a, Tvar a])]))
	       end

val arith1Typ = let val a = Symbol.symbol "a"
		in
		    Tabs ([a],  Tcon (Arrow, [Tvar a, Tvar a]))
		end
		    
val compTyp  = let val a = Symbol.symbol "a"
	       in
		   Tabs ([a],  Tcon (Arrow, [Tvar a, Tcon (Arrow, [Tvar a, Bool])]))
	       end
		   
val listConsTyp = let val a = Symbol.symbol "a"
		  in
		      Tabs ([a],  Tcon (Arrow, [Tvar a, Tcon (Arrow, [Tcon (Pair, [Tvar a]), 
								      Tcon(Pair, [Tvar a])])]))
		  end

val listOp1Typ  = let val a = Symbol.symbol "a"
		  in
		      Tabs ([a], Tcon (Arrow, [Tcon(Pair, [Tvar a]), Tvar a]))
		  end

val listOp2Typ  = let val a = Symbol.symbol "a"
		  in
		      Tabs ([a], Tcon (Arrow, [Tcon(Pair, [Tvar a]), Tcon (Pair, [Tvar a])]))
		  end

val generatorTyp = Tcon (Arrow, [Real, Real])

val iotaTyp      = Tcon (Arrow, [generatorTyp, 
				 Tcon(Arrow, [Real, Tcon(Arrow, [Real, Tcon(Arrow, 
									    [Real, Tcon (Vector, [Real])])])])])




end
