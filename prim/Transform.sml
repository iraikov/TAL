
(* 
 * Transform.sml
 *
 * System F primitive conversion transformation.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Transform: TRANSFORM =
struct

open Semant

exception ReduceError
exception TransformTypeError of CPS.typ
exception TransformExpError of CPS.exp
exception TransformValError of CPS.value
exception TransformProgramError of CPS.program


val (prim_transform_type,prim_transform_exp,prim_transform_program,prim_typebase) = 
    let 
	fun C (CPS.Nil)             = Nil
	  | C (CPS.Unit)            = Unit
	  | C (CPS.Bool)            = Bool
	  | C (CPS.Int)             = Int
	  | C (CPS.Real)            = Real
	  | C (CPS.Pair t)          = Pair (C t)
	  | C (CPS.Record sts)      = Record (map (fn(s,t) => (s, C t)) sts)
	  | C (CPS.Vector t)        = Vector (C t)
	  | C (CPS.Tvar a)          = Tvar a
	  | C (CPS.Arrow (a,ts,t))  = Arrow(a,map C ts, C t)

	val typebase = 
	    let
		val {symbols,env}  =  CPS.cps_typebase
		val env'  =  foldl (fn(s,E) => Env.enter (s,C (Env.look(s,env)),E)) (Env.empty) symbols
	    in 
		{symbols=symbols,env=env'}
	    end

	fun prim_v D T P (CPS.Proc (a, xs, e)) = 
	    let
		val T'      = foldl (fn((s,t),T) => Env.enter(s,C t,T)) T  xs
		val e'      = prim_e (a @ D) T' P e
	    in
		Proc(a,map (fn(s,t) => (s,C t)) xs,e')
	    end
	  | prim_v D T P (CPS.Var x)       = Var x
	  | prim_v D T P (CPS.NilVal)      = NilVal
	  | prim_v D T P (CPS.UnitVal)     = UnitVal
	  | prim_v D T P (CPS.BoolVal b)   = BoolVal b
	  | prim_v D T P (CPS.IntNum i)    = IntNum i
	  | prim_v D T P (CPS.RealNum r)   = RealNum r
	  | prim_v D T P (CPS.PairVal (v1,v2))     = 
            PairVal (prim_v D T P v1, prim_v D T P v2)
	  | prim_v D T P (CPS.RecordVal (ss,env))  = 
	    let 
		val vs' = map (prim_v D T P) (Env.listItems env)
	    in 
		RecordVal (ss, List.foldl (fn(s,V) => 
                                              let val v = Env.look(s,env)
						  val v' = prim_v D T P v
					      in
						  Env.enter (s,v',V)
					      end) Env.empty ss)
	    end
	  | prim_v D T P (CPS.VectorVal  v) =
	    let 
		val vs' = Vector.foldr (fn(v,vs) => (prim_v D T P v) :: vs) [] v
	    in 
		VectorVal (Vector.fromList vs')
	    end

	  | prim_v D T P (CPS.Vref (v,i))  = 
            Vref (prim_v D T P v, prim_v D T P i)

	  | prim_v D T P (CPS.Rsel (v,s))  = 
            Rsel (prim_v D T P v, s)

	  | prim_v D T P (x as CPS.Prim (v1 as CPS.Prim _,[],v2)) =
	    let 
		val v1' = prim_v D T P v1
		val v2' = prim_v D T P v2
	    in 
		case v1' of 
		    Prim(v,ts,vs) => Prim(v,ts,vs @ [v2'])
		  | _             => raise TransformValError x
	    end

	  | prim_v D T P (CPS.Prim (CPS.Var x,t,v2)) =
	    let 
		val v2' = prim_v D T P v2
	    in 
		Prim(Var x,map C t,[v2'])
	    end

	  | prim_v D T P (x as CPS.Prim _) = raise TransformValError x

	and prim_e  D T P (CPS.If0(v,e1,e2)) = 
	    If0(prim_v D T P v, prim_e D T P e1, prim_e D T P e2)
	    
	  | prim_e D T P (CPS.Halt(t,v))  = 
	    Halt (C t, prim_v D T P v)


	  | prim_e D T P (ex as CPS.Let(s,CPS.Prim(CPS.Var x,ts,v1),e)) =
	    let
		val v1' = prim_v D T P v1 

		val (x,ts',vs')  = (case Env.find(x,P) of
					    SOME (x,ts,vs) => (x,ts,vs @ [v1'])
					  | NONE  => (x,map C ts, [v1']))
		val p  = Prim(Var x,ts',vs')
		val t  = prim_Vt T p
	    in
		case t of
		    Arrow _ => prim_e D (Env.enter(s,t,T)) (Env.enter(s,(x,ts',vs'),P)) e
		  | _       => Let(s,p,prim_e D (Env.enter(s,t,T)) P e)
	    end

	  | prim_e D T P (CPS.Let(s,v,e)) =
	    let
		val v' = prim_v D T P v 
		val t  = prim_Vt  T v'
		val e' = prim_e D (Env.enter (s,t,T)) P e 
	    in
		Let (s,v',e')
	    end

	  | prim_e D T P (CPS.App (v,ts,vs)) =
	    let
		val v'  = prim_v D T P v 
		val vs' = map (prim_v D T P) vs
		val ts' = map C ts
	    in
		App(v',ts',vs')
	    end

	fun prim_e' D T e  = prim_e D T Env.empty e
	    
	fun prim_p ((s,t,e): CPS.program) = 
	    let val T = (#env typebase)
	    in 
		(s,C t,(prim_e' [] T e))
	    end
			     
			     
    in 
	(C, prim_e', prim_p, typebase)
    end
    

end

