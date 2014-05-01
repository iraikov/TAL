
(* 
 * Transform.sml
 *
 * System F continuation-passing style transformation.
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

exception TransformTypeError of SF.typ
exception TransformExpError of SF.exp
exception TransformTexpError of SF.exp
exception TransformPexpError of SF.exp
exception TransformProgramError of SF.program


val (cps_transform_type,cps_transform_exp,cps_transform_program,cps_typebase) = 
    let 
	fun K (SF.Nil)     = Nil
	  | K (SF.Unit)    = Unit
	  | K (SF.Bool)    = Bool
	  | K (SF.Int)     = Int
	  | K (SF.Real)    = Real
	  | K (SF.Tvar s)  = Tvar s
	  | K (SF.Tcon (SF.Pair, [t]))     = Pair (K t)
	  | K (SF.Tcon (SF.Record, flds))  = 
	    Record (map (fn(SF.Field (s,t)) => (s, K t)
			 | x  => raise TransformTypeError x 
				       before (print "K: x = "; SF.pptyp x; print "\n")) flds)
	  | K (SF.Tcon (SF.Vector, [t]))     = Vector (K t)
	  | K (SF.Tcon (SF.Arrow, [t1,t2]))  = Arrow ([], [K t1, N t2], Unit)
	  | K (SF.Tabs (a,t))                = Arrow (a, [N t], Unit)
	  | K x      = raise TransformTypeError x
			     before (print "K: x = "; SF.pptyp x; print "\n")
			     
	and N t      = Arrow ([], [K t], Unit)

	val typebase = 
	    let fun KP (SF.Nil)     = Nil
		  | KP (SF.Unit)    = Unit
		  | KP (SF.Bool)    = Bool
		  | KP (SF.Int)     = Int
		  | KP (SF.Real)    = Real
		  | KP (SF.Tvar s)  = Tvar s
		  | KP (SF.Tcon (SF.Pair, [t]))     = Pair (KP t)
		  | KP (SF.Tcon (SF.Record, flds))  = 
		    Record (map (fn(SF.Field (s,t)) => (s, KP t)
				 | x  => raise TransformTypeError x 
					       before (print "K: x = "; SF.pptyp x; print "\n")) flds)
		  | KP (SF.Tcon (SF.Vector, [t]))     = Vector (KP t)
		  | KP (SF.Tcon (SF.Arrow, [t1,t2]))  = Arrow ([], [K t1], KP t2)
		  | KP (x as SF.Tabs (a,t))           = let val t' = KP t 
							in case t' of Arrow (b,t1,t2) => Arrow (a @ b,t1,t2)
								    | _ => raise TransformTypeError x
							end
		  | KP x      = raise TransformTypeError x
				     before (print "KP: x = "; SF.pptyp x; print "\n")

		val {symbols,env}  =  SF.typebase
		val env'  =  foldl (fn(s,E) => Env.enter (s,KP (Env.look(s,env)),E)) (Env.empty) symbols
	    in 
		{symbols=symbols,env=env'}
	    end

	fun cps_const (SF.UNIT_CONST)    = UnitVal
	  | cps_const (SF.NIL_CONST  )   = NilVal
	  | cps_const (SF.REAL_CONST r)  = RealNum r
	  | cps_const (SF.INT_CONST i)   = IntNum i
	  | cps_const (SF.BOOL_CONST b)  = BoolVal b

	fun cps_t (SF.Texp  (t, SF.Const k))  = (fn c => App (c, [], [cps_const k]))
	  | cps_t (SF.Texp  (t, SF.Var s))    = (fn c => App (c, [], [Var s]))
	  | cps_t (SF.Tprim (t, SF.Var s))    = (fn c => App (c, [], [Var s]))
	  | cps_t (SF.Texp  (t, SF.If (e1, e2, e3)))  = 
	    (let val e1'      = cps' e1 
		 val e2'      = cps_t e2
		 val e3'      = cps_t e3 
	     in 
	      fn c => e1' (fn y => If0 (y, e2' c, e3' c))
	     end)

	  | cps_t (SF.Tprim (t, x as SF.App (e1 as SF.Tprim(SF.Tcon(SF.Arrow,[t1,t2]),_), e2)))  = 
	    (let
		 val (e1',tau) = (case e1 of SF.Tprim(_,SF.Tbox(s,tbs,_)) => 
					     (let val tau = map (fn(s,t) => K t) tbs
						  val x   = Var s
					      in
						  (fn k => k x,tau)
					      end)
					   | SF.Tprim(_,SF.Var s) => (fn k => k (Var s),[])
					   | _ => (cps' e1,[]))
				 
		 val e2' = cps' e2
		 val z   = Symbol.freshPrefix "cps10"
	     in
		 (fn c => (e1' (fn y1 => e2' (fn y2 => Let (z,Prim(y1,tau,y2),App(c,[],[Var z]))))))
	     end)

	  | cps_t (SF.Texp  (t, SF.App (e1 as SF.Texp(SF.Tcon(SF.Arrow,[t1,t2]),_), e2)))  = 
	    (let
		 val e1' = cps' e1
		 val e2' = cps' e2
	     in
		 (fn c => (e1' (fn y1 => e2' (fn y2 => App (y1, [], [y2, c])))))
	     end)

	  | cps_t (SF.Texp  (SF.Tcon (SF.Arrow, [t1,t2]), SF.Abs (x,_,e)))    = 
	    (let 
		 val e'  = cps_t e
		 val c'  = Symbol.freshPrefix "cps20"
	     in 
		 fn c => App (c, [], [Proc ([], [(x, K t1), (c', N t2)], e' (Var c'))])
	     end)

	  | cps_t (SF.Texp (t, SF.Let1 ((x,_,e1),e2)))    = 
	    (let 
		 val e1'  = cps' e1
		 val e2'  = cps_t e2
	     in 
		 fn c => e1' (fn y => Let(x,y,e2' c))
	     end)

	  | cps_t (SF.Texp  (t', SF.Tapp (e, ts)))   = 
	    (let 
		 val e' = cps' e
	     in 
		 fn c => (e' (fn y => App (y, map K ts, [c])))
	     end)

	  | cps_t (SF.Texp  (_, SF.Tlam (a, SF.Texp(t,e))))    = 
	    (let
		 val e' = cps_t e
		 val c' = Symbol.freshPrefix "cps30"
	     in
		 fn c => App (c, [], [Proc (a, [(c', N t)], e' (Var c'))])
	     end)

	  | cps_t (SF.Texp  (t, e as SF.Tlet _))    = 
	    cps_t (SF.esubst Env.empty e)

	  | cps_t (x as SF.Texp  (t, SF.Rcon ses))  = 
	    (let
		 val ts       = case t of SF.Tcon(SF.Record,ts) => 
					  map (fn(t) => 
						 case t of SF.Field(s,t) => t
							 | _ => raise TransformTexpError x) ts
					| _ => raise TransformTexpError x 
						     before (print  ("cps_t error: Rcon: x = "); 
							     SF.pp (SF.Exp x); 
							     print "\n";
							     print "t = "; SF.pptyp t;
							     print "\n")

		 val (ss,es') = ListPair.unzip (map (fn(s,e) => (s,cps' e)) ses)

		 val (ys,env) =  foldr (fn(s,(ys,env)) => 
					  let val y   =  Symbol.freshPrefix "cps40"
					  in
					      (y :: ys, Env.enter(s,Var y,env))
					  end) ([], Env.empty) ss
		 val r        = RecordVal (ss,env)

		 val yn       = List.last ys
		 val ys'      = List.take (ys, (length ys)-1)

		 val tn       = List.last ts
		 val ts'      = List.take (ts, (length ts)-1)

		 val (e1',es') = (case es' of 
				      (e1 :: es) => (e1,es)
				    | _          => raise TransformTexpError x 
							  before (print  ("cps_t error: Rcon: x = "); 
								  SF.pp (SF.Exp x); print "\n"))

		 val tys  = ListPair.zip (ts',ys')
	     in
	      fn c => e1'  (ListPair.foldr (fn ((t,y),e,ax) => 
						fn x => Let (y,x,e ax))
					   (fn x => Let (yn,x,App(c,[],[r]))) (tys,es'))
	     end)

	  | cps_t (SF.Texp  (t, x as SF.Vcon es))       = 
	    (let
		 val tv       = case t of SF.Tcon(SF.Vector,[tv]) => K tv
					| _                       => raise TransformTexpError x

		 val (ys,vs,es') =  foldr (fn(e,(ys,vs,es)) => 
					     let val y = Symbol.freshPrefix "cps50"
					     in
						 (y :: ys, (Var y) :: vs, (cps' e) :: es)
					     end) ([],[],[]) es
		 val v           = VectorVal (Vector.fromList vs)

		 val yn          = List.last ys
		 val ys'         = List.take (ys, (length ys)-1)

		 val tn          = tv
		 val ts'         = List.tabulate (length ys',fn(i) => tv)

		 val (e1,es')    = case es' of (e1 :: es) => (e1,es)
					     | _          => raise TransformTexpError (SF.Vcon es)

		 val tys  = ListPair.zip (ts',ys')

	     in
	      fn c => e1  (ListPair.foldr (fn ((t,y),e,ax) => (fn x => Let (y,x,e ax)))
					  (fn x => Let (yn,x,App(c,[],[v]))) (tys,es'))
	     end)

	  | cps_t (SF.Texp  (t, SF.Rsel (e,s)))     = 
	    (let
		 val e'  = cps' e
		 val z   = Symbol.freshPrefix "cps60"
	     in 
		 fn c => e' (fn y => Let(z,Rsel (y,s),App (c,[],[Var z])))
	     end)

	  | cps_t (SF.Texp  (t, SF.Vref (e,i)))     = 
	    (let val e'  = cps' e
		 val i'  = cps' i
		 val z   = Symbol.freshPrefix "cps70"
	     in 
	      fn c => e' (fn y1 => i' (fn y2 => Let(z,Vref (y1,y2),App (c, [], [Var z]))))
	     end)

	  | cps_t x = raise TransformTexpError x before (print  ("cps_t error: x = "); SF.pp (SF.Exp x); print "\n")

	and cps' (SF.Texp  (t, SF.Const c))      = (fn k => k (cps_const c))
	  | cps' (SF.Texp  (t, SF.Var s))        = (fn k => k (Var s))
	  | cps' (SF.Texp  (t, SF.If  (e1, e2, e3)))  = 
	    (let val e1'  = cps' e1 
		 val e2'  = cps_t e2
		 val e3'  = cps_t e3 
		 val z    = Symbol.freshPrefix "cps110"
		 val c    = Symbol.freshPrefix "cps120"
		 val tc   = Arrow ([],[K t],Unit)
	     in 
	      fn k => e1' (fn y => Let (c,Proc ([], [(z, K t)], k (Var z)),
					If0 (y, e2' (Var c), e3' (Var c))))
	     end)

	  | cps' (SF.Tprim  (t, x as SF.App (e1 as SF.Tprim(SF.Tcon(SF.Arrow,[t1,t2]),_), e2)))  = 
	    (let
		 val (e1',tau) = (case e1 of SF.Tprim(_,SF.Tbox(s,tbs,_)) => 
					       (let val tau = map (fn(s,t) => K t) tbs
						in
						    (fn k => k (Var s),tau)
						end)
					   | SF.Tprim(_,SF.Var s) => (fn k => k (Var s),[])
					   | _   => (cps' e1,[]))
		 val e2'   = cps' e2
		 val z     = Symbol.freshPrefix "cps130"
	     in
		 (fn k => (e1' (fn y1 => e2' (fn y2 => Let (z,Prim(y1,tau,y2), k (Var z))))))
	     end)

	  | cps' (SF.Texp  (t, SF.App (e1 as SF.Texp(SF.Tcon(SF.Arrow,[t1,t2]),_), e2)))  = 
	    (let
		 val e1'   = cps' e1
		 val e2'   = cps' e2
		 val z     = Symbol.freshPrefix "cps140"
	     in
		 (fn k => (e1' (fn y1 => e2' (fn y2 => App (y1, [], [y2, Proc ([], [(z, K t2)], k (Var z))])))))
	     end)

	  | cps' (SF.Texp  (SF.Tcon (SF.Arrow, [t1,t2]), SF.Abs (x,_,e)))    = 
	    (let 
		 val e' = cps_t e
		 val c  = Symbol.freshPrefix "cps150"
	     in 
		 fn k => k (Proc ([], [(x, K t1), (c, N t2)], e' (Var c)))
	     end)

	  | cps' (SF.Texp (t, SF.Let1 ((x,_,e1),e2)))    = 
	    (let
		 val e1' = cps' e1
		 val e2' = cps_t e2
		 val c   = Symbol.freshPrefix "cps155"
		 val z   = Symbol.freshPrefix "cps156"
	     in 
		 fn k => e1' (fn y => Let (c,Proc ([], [(z, K t)], k (Var z)),
					   Let(x,y,e2' (Var c))))
	     end)

	  | cps' (SF.Texp  (t, SF.Tapp (e, ts)))   = 
	    (let 
		 val e'  = cps' e
		 val z   = Symbol.freshPrefix "cps160"
	     in 
		 fn k => (e' (fn y => App (y, map K ts, [Proc ([], [(z, K t)], k (Var z))])))
	     end)

	  | cps' (SF.Texp  (_, x as SF.Tlam (a, SF.Texp(t,e))))    = 
	    (let
		 val e' = cps_t e
		 val c  = Symbol.freshPrefix "cps170"
	     in
		 fn k => k (Proc (a, [(c, N t)], e' (Var c)))
	     end)

	  | cps' (SF.Texp  (t, e as SF.Tlet _))    = cps' (SF.esubst Env.empty e)

	  | cps' (x as SF.Texp  (t, SF.Rcon ses))       = 
	    (let
		 val ts = case t of 
			      SF.Tcon(SF.Record,ts) => 
			      map (fn(t) => case t of 
						SF.Field(s,t) => t
					      | _ => raise TransformTexpError x) ts
			    | _ => raise TransformTexpError x
					 before (print  ("cps' error: Rcon: x = "); 
						 SF.pp (SF.Exp x); print "\n")

		 val (ss,es') = ListPair.unzip (map (fn(s,e) => (s,cps' e)) ses)

		 val (ys,env) =  foldr (fn(s,(ys,env)) => 
					  let val y = Symbol.freshPrefix "cps180"
					  in
					      (y :: ys, Env.enter(s,Var y,env))
					  end) ([], Env.empty) ss

		 val r        = RecordVal (ss,env)

		 val yn       = List.last ys
		 val ys'      = List.take (ys, (length ys)-1)

		 val tn       = List.last ts
		 val ts'      = List.take (ts, (length ts)-1)

		 val (e1,es') = case es' of (e1 :: es) => (e1,es)
					  | _          => raise TransformPexpError x

		 val tys  = ListPair.zip (ts',ys')

	     in
	      fn k => e1  (ListPair.foldr (fn ((t,y),e,ax) => 
					       fn x => Let (y,x,e ax))
					  (fn x => Let (yn,x,k r)) (tys,es'))

	     end)

	  | cps' (SF.Texp  (t, x as SF.Vcon es))       = 
	    (let
		 val tv       = case t of SF.Tcon(SF.Vector,[tv]) => K tv
					| _                     => raise TransformTexpError x

		 val (ys,vs,es') =  foldr (fn(e,(ys,vs,es)) => 
					     let val y = Symbol.freshPrefix "cps190"
					     in
						 (y :: ys, (Var y) :: vs, (cps' e) :: es)
					     end) ([],[],[]) es
		 val v           = VectorVal (Vector.fromList vs)

		 val yn          = List.last ys
		 val ys'         = List.take (ys, (length ys)-1)

		 val tn          = tv
		 val ts'         = List.tabulate (length ys',fn(i) => tv)

		 val (e1,es')    = case es' of (e1 :: es) => (e1,es)
					     | _          => raise TransformPexpError (SF.Vcon es)

		 val tys  = ListPair.zip (ts',ys')
	     in
	      fn k => e1  (ListPair.foldr (fn ((t,y),e,ax) => (fn x => Let (y,x,e ax)))
					  (fn x => Let (yn,x,k v)) (tys,es'))
	     end)

	  | cps' (SF.Texp  (t, SF.Rsel (e,s)))     = 
	    (let
		 val e' = cps' e
		 val z   = Symbol.freshPrefix "cps200"
	     in 
		 fn k => e' (fn y => Let(z,Rsel (y,s),k (Var z)))
	     end)

	  | cps' (SF.Texp  (t, SF.Vref (e,i)))     = 
	    (let val e'  = cps' e
		 val i'  = cps' i
		 val z   = Symbol.freshPrefix "cps210"
	     in 
	      fn k => e' (fn y1 => i' (fn y2 => Let(z,Vref (y1,y2),k (Var z))))
	     end)

	  | cps' x                            = 
	    raise TransformPexpError x  before (print ("cps' error: x = "); SF.pp (SF.Exp x); print "\n")
						      

	fun cps (SF.Texp (t,e)) = 
	    (let
		val e' = cps' (SF.Texp(t,e))
	    in 
		e' (fn y => Halt (K t, y))
	    end)

	  | cps x               = raise TransformExpError x


	fun cps_p (p as (s,vs,t,e)) = 
	    let 
		fun transform_p body ((s,t,e) :: rest) = 
		    SF.Let1 ((s,t,e),transform_p body rest)
		  | transform_p body ([]) = body

		val e     = transform_p e vs

		val (t,e) = case SF.elab (#env SF.typebase) e of 
				SF.Texp(t,e) => (t,SF.Texp(t,e))
			      | _            => raise TransformProgramError p

		val e'  = cps (e)
	    in 
		(s,K t,e')
	    end



    in 
	(K, cps, cps_p, typebase)
    end
    

end
