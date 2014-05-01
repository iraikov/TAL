
(* 
 * Type.sml
 *
 * System F continuation-passing style type definitions.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Type: TYPE =
struct

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
		  
fun fcmp ((s1,t1),(s2,t2)) = (Symbol.compare (s1,s2))

					      
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
    
and  tsubst  T (Tvar a)            = (case Env.find (a, T) of 
					  SOME t => t
					| NONE   => Tvar a)
   | tsubst  T (Arrow (a,ts,u))    =
     let
	 val ss  = List.tabulate (length a, fn(x) => Symbol.freshPrefix "cpsty")
	 val T'  = compose (a,ss,T)
     in
	 Arrow (ss, map (tsubst T') ts, tsubst T' u)
     end
   | tsubst T (Pair t)            = Pair (tsubst T t)
   | tsubst T (Vector t)          = Vector (tsubst T t)
   | tsubst T (Record sts)        = Record (map (fn(s,t) => (s, tsubst T t)) sts)
   | tsubst T t                   = t


fun unify (Nil, Nil)           = true
  | unify (Unit, Unit)         = true
  | unify (Bool, Bool)         = true
  | unify (Int, Int)           = true
  | unify (Real, Real)         = true 
  | unify (Pair u, Pair v)     = unify (u,v)
  | unify (Vector u, Vector v) = unify (u,v)
  | unify (Record u, Record v) = 
    (ListPair.all (fn((x,u),(y,v)) => 
		     (x=y andalso unify (u,v)))
		  (ListMergeSort.uniqueSort fcmp u, 
		   ListMergeSort.uniqueSort fcmp v))
  | unify (Arrow (a,u,t), Arrow (a',u',t')) = 
    (if (List.length a) = (List.length a') 
     then (let val ss  = List.tabulate (length a, fn(x) => Symbol.freshPrefix "cpsty")
	       val T  =  compose (a,ss,Env.empty)
	       val T' =  compose (a',ss,Env.empty)
	   in 
	       (ListPair.all (fn(u,u') => unify (tsubst T u, tsubst T' u')) (u, u')) 
	       andalso (unify (tsubst T t, tsubst T' t'))
	   end)
     else false)
  | unify (Tvar s, Tvar s')    = s=s'
  | unify (_, _)               = false


fun cps_pptyp' ty =
    let 
	fun ppSymbol (s) = Pretty.text (Symbol.name s)

	fun ppField (s,t) = let open Pretty in
				binop 2 (ppSymbol s, text ":", ppType t)
			    end
			     
	and ppType x = 
	    let open Pretty in 
		case x of 
		    Nil               => text "Nil"
		  | Unit              => text "Unit"
		  | Bool              => text "Bool"
		  | Int               => text "Int"
		  | Real              => text "Real"
				 
		  | (Pair ty) => 
		    group (connect (text "Pair", ppType ty))
					
		  | (Record flds) => 
		    group (connect (text "Record", 
				    block {l=brace L, r=brace R} 2 
					  (list 2 (ppField,comma) flds)))
					
		  | (Vector ty) => 
		    group (connect (text "Vector", ppType ty))
					
		  | (Arrow (a,xs,t))  => 
		    binop 2 (group (connect (if (not (List.null a))
					     then block {l=angle L,r=angle R} 2
							(list 2 (ppSymbol,comma) a)
					     else empty(),
					     block {l=angle L,r=angle R} 2
						   (list 2 (ppType,comma) xs))),
			     text "->",
			     ppType t)

				       
		  |  (Tvar x)      => block {l=brace L, r=brace R} 2 (ppSymbol x)

	    end
    in
	ppType ty
    end

	
fun cps_pptyp ty = Pretty.printf TextIO.stdOut (Pretty.format 60 (cps_pptyp' ty))

end
