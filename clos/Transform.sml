
(* 
 * Transform.sml
 *
 * System F polymorphic closure conversion.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Transform :> TRANSFORM =
struct

open Semant

exception TransformError


val code_fld   = Symbol.symbol "code"
val env_fld    = Symbol.symbol "env"


val (clos_transform_type,clos_transform_exp,clos_transform_program,clos_typebase) = 
    let 
	fun C (Prim.Nil)             = Nil
	  | C (Prim.Unit)            = Unit
	  | C (Prim.Bool)            = Bool
	  | C (Prim.Int)             = Int
	  | C (Prim.Real)            = Real
	  | C (Prim.Pair t)          = Pair (C t)
	  | C (Prim.Record sts)      = Record (foldr (fn((s,t),(ss,env)) => (s::ss,Env.enter(s,C t,env)))
						     ([],Env.empty) sts)
	  | C (Prim.Vector t)        = Vector (C t)
	  | C (Prim.Tvar a)          = Tvar a
	  | C (Prim.Arrow (a,ts,t))  = 
	    let val b      = Symbol.freshPrefix "clos1" 
	    in 
		Exists (b, Record (foldr (fn((s,t),(ss,env)) => (s::ss,Env.enter(s,t,env)))
					 ([],Env.empty)
					 [(code_fld, Arrow (a, (Tvar b) :: (map C ts), Unit)),
					  (env_fld, Tvar b)]))
	    end


	val typebase = 
	    let 
		fun CP (Prim.Nil)              = Nil
		  | CP (Prim.Unit)             = Unit
		  | CP (Prim.Bool)             = Bool
		  | CP (Prim.Int)              = Int
		  | CP (Prim.Real)             = Real
		  | CP (Prim.Pair t)           = Pair (CP t)
		  | CP (Prim.Record sts)       = Record (foldr (fn((s,t),(ss,env)) => (s::ss,Env.enter(s,C t,env)))
							      ([],Env.empty) sts)
		  | CP (Prim.Vector t)         = Vector (CP t)
		  | CP (Prim.Tvar a)           = Tvar a
		  | CP (Prim.Arrow (a,ts,t))   = Arrow(a,map C ts,CP t)

		val {symbols,env} =  Prim.prim_typebase
		val env'      =  foldl (fn(s,E) => 
					  let val t = Env.look(s,env)
					  in  Env.enter (s,{s=s,t=CP t},E) end) (Env.empty) symbols
	    in 
		env'
	    end


	fun clos_t H D T v = clos_Vt H D T typebase v

	datatype usedef = USE of symbol * typ * int
			| DEF of symbol * typ * int

	type usedefenv = usedef Env.env


	fun clos_v_def_use (v,UD) =
	    (case v of
		 Var x => (case Env.find (x,UD) of
			       SOME (USE (x,t,i)) => Env.enter(x,USE (x,t,i+1),UD)
			     | _ => UD)
	       | PairVal (v1,v2)   => foldl clos_v_def_use UD [v1,v2]
	       | RecordVal (_,env) => foldl clos_v_def_use UD (Env.listItems env)
	       | VectorVal vv      => Vector.foldl clos_v_def_use UD vv
	       | Vref (v,i)        => foldl clos_v_def_use UD [v,i]
	       | Rsel (v,f)        => clos_v_def_use (v,UD)
	       | Pack (_,v,_)      => clos_v_def_use (v,UD)
	       | Tapp (v,_)        => clos_v_def_use (v,UD)
	       | Prim (v,_,vs)     => foldl clos_v_def_use UD (v :: vs)
	       | _ => UD)

	fun clos_upd_def_use (s,UD) =
	    (case Env.find (s,UD) of
		 SOME (USE (s,t,i)) => Env.enter(s,DEF (s,t,i),UD)
	       | _ => UD)

	fun clos_e_def_use (x,UD) =
	    (case x of
		 App (v,vs)    => foldl clos_v_def_use UD (v :: vs)
	       | Halt (t,v)    => clos_v_def_use (v,UD)
	       | If0 (v,e1,e2) => foldl clos_e_def_use (clos_v_def_use (v,UD)) [e1,e2]
	       | Let (s,v,e)   => let val UD = clos_upd_def_use(s,UD)
				  in clos_e_def_use (e,clos_v_def_use (v,UD)) end
	       | Unpack (a,x,v,e)  => let val UD = clos_upd_def_use(x,UD)
				      in clos_e_def_use (e,clos_v_def_use (v,UD)) end)


	fun clos_app H D T (v,ts,vs) =
	    let
		val a_env  = Symbol.freshPrefix "a_env"
			     
		val x_code = Symbol.freshPrefix "x_code"
		val x_env  = Symbol.freshPrefix "x_env"
			     
		val z      = Symbol.freshPrefix "clos3"
			     
		val e      = Unpack (a_env, z, v, 
				     Let(x_code,Rsel (Var z, code_fld),
					 Let (x_env, Rsel (Var z, env_fld), 
					      App (foldl (fn(t,e) => Tapp (e, t)) (Var x_code) ts, 
						   (Var x_env) :: vs))))
	    in
		(H,e)
	    end


	and clos_proc H D T (a,xs,e) = 
	    let
		val T'      = foldl (fn((s,t),T) => Env.enter(s,{s=s,t=C t},T)) T  xs
		val (H',e') = clos_e H (a @ D) T' e

 
		val UD      = let val UD = Env.map (fn({s,t}) => USE (s,t,0)) T
			      in
				  if (Env.numItems UD) > 0
				  then clos_e_def_use (e',UD)
				  else UD
			      end

		val Titems  = List.mapPartial (fn(x) => let val (s,t,i) = (case x of USE x => x
									     | DEF x => x)
							in		    
							    if i>0 then SOME{s=s,t=t} else NONE 
							end) 
					      (Env.listItems UD)


		val x_env   =  Symbol.freshPrefix "x_env"

		val t_env   =  if List.null Titems 
			       then Unit
			       else Record (foldr (fn({s,t},(ss,env)) => (s::ss,Env.enter(s,t,env))) 
					     ([],Env.empty) Titems)
		val t_code  =  Prim.Arrow (a, map (fn(s,t) => t) xs, Prim.Unit)

		val v_env   =  if List.null Titems 
			       then UnitVal
			       else RecordVal (map (fn({s,t}) => s) Titems,
					       foldl (fn({s,t},V) => Env.enter (s,Var s,V)) Env.empty Titems)

		val b       =  D
		val xs'     =  (x_env, t_env) :: (map (fn(s,t) => (s,C t)) xs)
			
		val label   = Symbol.freshPrefix "label"
		val block   = (label, b @ a, xs', 
			       foldr (fn({s,t},e) => Let(s,Rsel (Var x_env,s),e)) e' Titems)

		val H'      = Env.enter(label,block,H')

		val v_label = Label label
       
		val closure = [(code_fld,  foldl (fn(b,ax) => Tapp (ax, Tvar b)) v_label b), 
			       (env_fld,   v_env)]
	    in
		(H', 
		 Pack (t_env, 
		       RecordVal ([code_fld, env_fld], 
				  foldl (fn((s,v),V) => Env.enter (s,v,V)) Env.empty closure), 
		       C t_code))
	    end

	and clos_v H D T (Prim.Proc (a, xs, e)) = 
	    clos_proc H D T (a,xs,e)

	  | clos_v H D T (Prim.Var x)       = (H, Var x)
	  | clos_v H D T (Prim.NilVal)      = (H, NilVal)
	  | clos_v H D T (Prim.UnitVal)     = (H, UnitVal)
	  | clos_v H D T (Prim.BoolVal b)   = (H, BoolVal b)
	  | clos_v H D T (Prim.IntNum i)    = (H, IntNum i)
	  | clos_v H D T (Prim.RealNum r)   = (H, RealNum r)
	  | clos_v H D T (Prim.PairVal (v1,v2)) = 
	    let val (H',v1') = clos_v H D T v1
		val (H',v2') = clos_v H' D T v2 
	    in 
		(H', PairVal (v1', v2'))
	    end
	  | clos_v H D T (Prim.RecordVal (ss,env)) = 
	    let 
		val (H',env') = foldl (fn(s,(H',env')) => let val v = Env.look(s,env)
							      val (H',v') = clos_v H' D T v
							  in 
							      (H',Env.enter(s,v',env'))
							  end)
				      (H,Env.empty) ss
	    in 
		(H', RecordVal (ss, env'))
	    end
	  | clos_v H D T (Prim.VectorVal v) =
	    let 
		val (H',vs') = Vector.foldr (fn(v,(H',vs)) => let val (H',v') = clos_v H' D T v
							      in 
								  (H',v' :: vs)
							      end) 
					    (H,[]) v
	    in 
		(H', VectorVal (Vector.fromList vs'))
	    end

	  | clos_v H D T (Prim.Vref (v,i))  =   
	    let
		val (H',v')  = clos_v H D T v 
		val (H',i')  = clos_v H' D T i 
	    in
		(H', Vref (v',i'))
	    end
	    
	  | clos_v H D T (Prim.Rsel (v,s))  =
	    let
		val (H',v')  = clos_v H D T v 
	    in
		(H', Rsel (v',s))
	    end

	  | clos_v H D T (Prim.Prim (v1,ts,vs)) =
	    let val (H',v1') = clos_v H D T v1
		val (H',vs') = foldr (fn(v,(H',vs)) => let val (H',v') = clos_v H' D T v
						       in 
							   (H',v' :: vs)
						       end) 
				     (H,[]) vs
	    in 
		(H', Prim(v1',map C ts,vs'))
	    end


	and clos_e  H D T (Prim.If0(v,e1,e2)) = 
	    let val (H',v')  = clos_v H D T v
		val (H',e1') = clos_e H' D T e1
		val (H',e2') = clos_e H' D T e2
	    in 
		(H', If0(v', e1', e2'))
	    end
	    
	  | clos_e H D T (Prim.Halt(t,v))  = 
	    let val (H',v') = clos_v H D T v
	    in 
		(H', Halt (C t, v'))
	    end

	  | clos_e H D T (Prim.Let(s,v,e)) =
	    let
		val (H',v') = clos_v H D T v 
		val t  = clos_t H' D T v'
		val (H',e') = clos_e H' D (Env.enter (s,{s=s,t=t},T)) e 
	    in
		(H', Let (s,v',e'))
	    end

	  | clos_e H D T (Prim.App (v,ts,vs)) =
	    let
		
		val (H',v') = clos_v H D T v
			      
		val (H',vs') = foldr (fn(v,(H',vs)) => let val (H',v') = clos_v H' D T v
						       in (H',v' :: vs) end) 
				     (H',[]) vs
	    in
		clos_app H' D T (v',map C ts,vs')
	    end

	    
	fun clos_p ((s,t,e):Prim.program) = 
	    let val (h,e') = clos_e Env.empty [] Env.empty e
	    in 
		(s, C t, h, e')
	    end

    in 
	(C,clos_e,clos_p,typebase)
    end
end
