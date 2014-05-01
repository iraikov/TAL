
(* 
 * Type.sml
 *
 * System F type definitions.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 *
 *)

structure Type : TYPE =
struct

    open Syntax

    type typenv = typ Env.env

    fun pptyp' ty =
	let 
	    fun ppSymbol (s) = Pretty.text (Symbol.name s)

	    fun ppType x =
		(let open Pretty in
		    (case x of 
			 Nil     => text "Nil"
		       | Unit    => text "Unit"
		       | Bool    => text "Bool"
		       | Int     => text "Int"
		       | Real    => text "Real"
		       | Pair    => text "Pair"
		       | Arrow   => text "->"
		       | Record  => text "Record"
		       | Vector  => text "Vector"
		       | (Field (s,t)) => binop 2 (ppSymbol s, text ":", ppType t)
					 
		       | (Tvar s)      => text ("Tvar " ^ (Symbol.name s))
					 
		       | (Tabs (ss,ty)) => 
			 binop 2 (connect (text "Tabs",
					   block {l=angle L, r=angle R} 2 
						 (list 2 (ppSymbol,comma) ss)),
				  text "=>", ppType ty)
			 
					  
		       | (Tcon (Arrow,[t1,t2]))  => 
			 binop 2 (ppType t1, text "->", ppType t2)

		       | (Tcon (ty,ts))  => 
			 group (connect (text "Tcon",
					 block {l=paren L,r=paren R} 2
						      (connect (ppType ty,
								block {l=angle L,r=angle R} 2
								      (list 2 (ppType,comma) ts)))))
                    )
		 end)
	in
	    ppType ty
	end

	
    fun pptyp sexp = Pretty.printf TextIO.stdOut (Pretty.format 60 (pptyp' sexp))



    fun constant n = case n of Syntax.UNIT_CONST       => Unit
			     | Syntax.NIL_CONST        => Nil
			     | Syntax.BOOL_CONST _     => Bool
			     | Syntax.INT_CONST  _     => Int
			     | Syntax.REAL_CONST _     => Real
					      
					      
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


    val typebase = 
	{symbols=[Symbol.symbol "add",  
		  Symbol.symbol "sub",  
		  Symbol.symbol "mul",  
		  Symbol.symbol "div",  
		  Symbol.symbol "eq",   
		  Symbol.symbol "less", 
		  Symbol.symbol "cons", 
		  Symbol.symbol "car",  
		  Symbol.symbol "cdr",  
	          
		  Symbol.symbol "or",   
		  Symbol.symbol "and",  
		  Symbol.symbol "not",  
	          
		  Symbol.symbol "pow",
		  Symbol.symbol "round",
		  Symbol.symbol "exp",  
		  Symbol.symbol "sqrt", 
		  Symbol.symbol "sin",  
		  Symbol.symbol "cos",  
		  Symbol.symbol "tan",  
		  Symbol.symbol "asin", 
		  Symbol.symbol "acos", 
		  Symbol.symbol "atan", 
		  Symbol.symbol "ln",   
		  Symbol.symbol "sinh", 
		  Symbol.symbol "cosh", 
		  Symbol.symbol "tanh", 
	          
		  Symbol.symbol "iota", 
	          
		  Symbol.symbol "read", 
		  Symbol.symbol "write"],

	 env=List.foldl (fn ((x: symbol,t: typ),env: typenv) => Env.enter (x,t,env)) (Env.empty: typenv)
			[(Symbol.symbol "add",   arithTyp),
			 (Symbol.symbol "sub",   arithTyp),
			 (Symbol.symbol "mul",   arithTyp),
			 (Symbol.symbol "div",   arithTyp),
			 (Symbol.symbol "eq",    compTyp),
			 (Symbol.symbol "less",  compTyp),
			 (Symbol.symbol "cons",  listConsTyp),
			 (Symbol.symbol "car",   listOp1Typ),
			 (Symbol.symbol "cdr",   listOp2Typ),
			 
			 (Symbol.symbol "or",     Tcon (Arrow, [Bool, Tcon(Arrow, [Bool, Bool])])),
			 (Symbol.symbol "and",    Tcon (Arrow, [Bool, Tcon(Arrow, [Bool, Bool])])),
			 (Symbol.symbol "not",    Tcon (Arrow, [Bool, Bool])),

			 
			 (Symbol.symbol "pow",   Tcon (Arrow, [Int, mathTyp])),
			 (Symbol.symbol "round", Tcon (Arrow, [Real, Int])),
			 (Symbol.symbol "exp",   mathTyp),
			 (Symbol.symbol "sqrt",  mathTyp),
			 (Symbol.symbol "sin",   mathTyp),
			 (Symbol.symbol "cos",   mathTyp),
			 (Symbol.symbol "tan",   mathTyp),
			 (Symbol.symbol "asin",  mathTyp),
			 (Symbol.symbol "acos",  mathTyp),
			 (Symbol.symbol "atan",  mathTyp),
			 (Symbol.symbol "ln",    mathTyp),
			 (Symbol.symbol "sinh",  mathTyp),
			 (Symbol.symbol "cosh",  mathTyp),
			 (Symbol.symbol "tanh",  mathTyp),
			 
			 (Symbol.symbol "iota",  iotaTyp),
			 
			 (Symbol.symbol "read",   Tcon (Arrow, [Int, Real])),
			 (Symbol.symbol "write",  Tcon (Arrow, [Int, Tcon (Arrow, [Real, Unit])]))]}



end
