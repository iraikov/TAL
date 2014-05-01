
(* 
 * Transform.sml
 *
 * System F allocation transformation.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Transform:> TRANSFORM =
struct

open Semant

exception TransformError

val (alloc_transform_type,alloc_transform_exp,alloc_transform_program,alloc_typebase) = 
    let 
	fun A (Clos.Nil, (U,P))     = (Nil,  (U,P))
	  | A (Clos.Unit, (U,P))    = (Unit, (U,P))
	  | A (Clos.Bool, (U,P))    = (Bool, (U,P))
	  | A (Clos.Int,  (U,P))    = (Int,  (U,P))
	  | A (Clos.Real, (U,P))    = (Real, (U,P))
	  | A (Clos.Tvar a, (U,P))  = ((Tvar a), (U,P))

	  | A (Clos.Arrow (a,ts,t), (U,P)) = 
	    let val (ts',(U',P')) = foldr (fn(t,(ts,(U,P))) => 
					     let val (t',(U',P')) = A (t,(U,P))
						 val (k,U')  = ttEnter(t',U')
					     in ((t',k) :: ts,(U',P')) end) ([],(U,P)) ts
		val (t',(U',P'))  = A (t,(U',P'))
	    in 
		((Arrow(a,ts',t')), (U',P'))
	    end

	  | A (Clos.Exists (s,t), (U,P)) = 
	    let val (t',(U',P')) = A (t,(U,P))
		val (k,U')  = ttEnter(t',U')
	    in 
		((Exists(s,(t',k))), (U',P'))
	    end

	  | A (Clos.Pair t, (U,P)) = 
	    let val (t',(U',P'))  = A (t,(U,P))
		val (p,P')   = phEnter (Vector.fromList [IN,IN],P')
		val (k,U')   = ttEnter(t',U')
	    in 
		((Pair ((t',k),p)), (U',P'))
	    end


	  | A (Clos.Record (ss,env), (U,P)) = 
	    let val (fs,(U',P'),_)  = 
		    foldr (fn(s,(ax,(U,P),i))=> 
			     let val t = Env.look (s,env)
				 val (t',(U',P')) = A (t,(U,P))
				 val (k,U')       = ttEnter(t',U')    
			     in 
				 ((s,(t',k),i) :: ax,(U',P'),i-1)
			     end) ([],(U,P),(List.length ss)-1) ss

		val p = map (fn(s) => IN) ss
		val (p,P') = phEnter (Vector.fromList p,P')
	    in 
		((Record (fs,p)), (U',P'))
	    end

	  | A (Clos.Vector t, (U,P)) = 
	    let val (t',(U',P'))  = A (t,(U,P))
		val (p,P')   = phEnter (Vector.fromList [IN],P')
		val (k,U')   = ttEnter(t',U')
	    in 
		((Vector ((t',k),p)), (U',P'))
	    end


	val cons_s = Symbol.symbol "cons"
	val cons_p = (fn([(t,k)],(U,P)) => 
			let
		       	    val (p,P') = phEnter(Vector.fromList [UN,UN], P)
			    val ty     = Pair ((t,k),p)
			    val (k,U') = ttEnter(ty,U)
			in
			    (k,ty,(U',P'))
			end
		      | _ => raise TransformError)

	val iota_s = Symbol.symbol "iota"
	val iota_p = (fn([],(U,P))  => 
			let
		       	    val (p,P') = phEnter(Vector.fromList [UN], P)
			    val (k,U') = ttEnter(Real,U)

			    val ty     = (Vector ((Real,k),p))
			    val (k,U') = ttEnter(ty,U')
			in
			    (k,ty,(U',P'))
			end
		      | _ => raise TransformError)

	val typebase = 
	    let val env  =  Clos.clos_typebase
		val (ss,env',(U,P)) =  foldl (fn({s,t},(ss,E,(U,P))) => 
						let val (t',(U',P')) = A (t,(U,P))
						    val (k,U')  = ttEnter (t',U')
						in 
						    (s::ss, Env.enter (s,t',E), (U',P'))
						end)
					     ([],Env.empty,(ttBase,phEmpty))
					     (Env.listItems env)
				
		val cons_t = Env.look(cons_s, env')
		val iota_t = Env.look(iota_s, env')
	
		fun ttyp ((Arrow(_,_,t))) = ttyp t
		  | ttyp t = t

		val (tt,U') = foldr (fn(t,(tt,U)) => 
				       case t of 
					   (Arrow(a,[ts],t)) => 
					   (let val t' = ttyp t
						val (k,U') = ttEnter (t',U)
					    in
						(((Arrow(a,[(t',k)], (Arrow([],[ts],t))))) :: tt, U')
					    end)
					 | _ => raise TransformError) ([],U) [cons_t, iota_t]
			      
		val env'' = ListPair.foldl (fn(s,t,env) => Env.enter(s,t,env)) env' 
					   ([cons_s,iota_s], tt)
	    in 
		{symbols=ss,env=env'',phimap=P,utmap=U'}
	    end

	val allocbase = 
	    let 
		val ss = [cons_s, iota_s]
		val pp = [cons_p, iota_p]
	    in 
		foldl (fn((s,t),env) => Env.enter (s,t,env)) Env.empty (ListPair.zip (ss,pp))
	    end

	fun alloc_t H P T (d,v)  = 
	    let
		val (P',T') = alloc_Dt H P T d
	    in 
		alloc_Vt H P' T' v
	    end

	fun alloc_prim (U: utmap) (H: heap) (P: phimap) (p,ts) = 
	    (case Env.find (p,allocbase) of
		 SOME pp => (let 
				 val (k,t,(U',P'))  = pp (ts,(U,P))
				 val l       = Symbol.freshPrefix "primheap"
				 val H'      = Env.enter (l,((l,k),t,NONE),H)
				 val d  = case t of 
					       (Pair(t,_))    => [Lmalloc (l,t)]
					    |  (Vector(t,_))  => [Vmalloc (l,t,0)]
					    |  (Record(fs,_)) => [Rmalloc (l,fs)]
					    | _ => raise TransformError
			     in 
				SOME (d,l,(U',H',P'))
			     end)
	       | NONE => NONE)



	fun alloc_v (Clos.NilVal, (U,H,P,T,Tt)) = 
	    let
		val (k,U') = ttEnter (Nil,U)
	    in 
		(NilVal, k, Nil, [], (U',H,P))
	    end

	  | alloc_v (Clos.UnitVal, (U,H,P,T,Tt)) = 
	    let
		val (k,U') = ttEnter (Unit,U)
	    in 
		(UnitVal, k, Unit, [], (U',H,P))
	    end

	  | alloc_v (Clos.BoolVal b, (U,H,P,T,Tt)) = 
	    let
		val (k,U') = ttEnter (Bool,U)
	    in 
		(BoolVal b, k, Bool, [], (U',H,P))
	    end

	  | alloc_v (Clos.IntNum i, (U,H,P,T,Tt)) = 
	    let
		val (k,U') = ttEnter (Int,U)
	    in 
		(IntNum i, k, Int, [], (U',H,P))
	    end

	  | alloc_v (Clos.RealNum r, (U,H,P,T,Tt)) = 
	    let
		val (k,U') = ttEnter (Real,U)
	    in 
		(RealNum r, k, Real, [], (U',H,P))
	    end

	  | alloc_v (Clos.Var x, (U,H,P,T,Tt)) = 
	    let 
		val ty     = Env.look(x,T)
		val (k,U') = ttEnter(ty,U)
	    in
		(Var x, k, ty, [], (U',H,P))
	    end

	  | alloc_v (Clos.Label l, (U,H,P,T,Tt)) = 
	    let 
		val (_,ty,_) = Env.look(l,H)
		val (k,U')   = ttEnter(ty,U)
	    in
		(Label (l), k, ty, [], (U',H,P))
	    end
	    
	  | alloc_v (Clos.Pack (t,v,at), (U,H,P,T,Tt))   = 
	    let 
		val (v',vk,_,d,(U',H',P')) = alloc_v (v,(U,H,P,T,Tt))
		val (t',(U',P'))   = A(t,(U',P'))
		val (at',(U',P'))  = A(at,(U',P'))
		val (kt,U')   = ttEnter(t',U')
		val (kat,U')  = ttEnter(at',U')
		val x         = Pack ((t',kt), (v',vk), (at',kat))
		val ty        = alloc_t H' P' T (d,x)
		val (k,U')    = ttEnter(ty,U')
	    in 
		(x, k, ty, d, (U',H',P'))
	    end

	  | alloc_v (Clos.Tapp (v,t), (U,H,P,T,Tt))      = 
	    let 
		val (v',_,_,d,(U',H',P')) = alloc_v (v,(U,H,P,T,Tt))
		val (t',(U',P'))  = A (t,(U',P'))
		val x        = Tapp(v',t')
		val ty       = alloc_t H' P' T (d,x)
		val (k,U')   = ttEnter(ty,U')
	    in 
		(x, k, ty, d, (U',H',P'))
	    end

	  | alloc_v (Clos.Prim (Clos.Var p,ts,vs), (U,H,P,T,Tt))      = 
	    let 
		val (ts',(U',P')) = foldr (fn(t,(ax,(U,P))) => 
					     let val (t',(U',P')) = A(t,(U,P))
						 val (k,U')   = ttEnter(t',U')
					     in ((t',k)::ax,(U',P')) end) ([],(U,P)) ts


		val (vs',ds,(U',H',P'))  = 
		    foldr (fn(v,(vs,ds,(U,H,P))) => 
			     let
				 val (v',_,_,d,(U',H',P')) = alloc_v (v,(U,H,P,T,Tt))
			     in 
				 (v' :: vs, d @ ds, (U',H',P'))
			     end) ([],[],(U',H,P')) vs

		val (v',ds',(U',H',P')) = 
		    case alloc_prim U' H' P' (p,ts') of 
			NONE  => (Prim (Var p, ts', vs'), ds, (U',H',P'))
		      | SOME (d1,l,(U'',H'',P'')) => (Prim (Var p, ts', (Label l) :: vs'), 
						      d1 @ ds, (U'',H'',P''))

		val ty      = alloc_t H' P' T (ds',v')
		val (k,U')  = ttEnter (ty,U')

	    in 
		(v', k, ty, ds',(U',H',P'))
	    end

	  | alloc_v (Clos.Prim (_,_,_), (U,H,P,T,Tt)) = 
	    raise TransformError before print "alloc_v: Prim\n"

	  | alloc_v (Clos.PairVal (v1,v2), (U,H,P,T,Tt)) = 
	    let
		val car  = Symbol.symbol "car"
		val cdr  = Symbol.symbol "cdr"

		val l  = Symbol.freshPrefix "pair"

		val (v1',_,_,d1,(U',H',P'))  = alloc_v (v1,(U,H,P,T,Tt))
		val (v2',_,_,d2,(U',H',P'))  = alloc_v (v2,(U',H',P',T,Tt)) 
				
		val lty     = alloc_t H' P' T (d1,v1')
		val (p,P')  = phEnter (Vector.fromList [UN,UN],P)
		val (kl,U') = ttEnter (lty,U')
		val ty      = Pair ((lty,kl),p)
		val (k,U')  = ttEnter (ty,U')
		val H'      = Env.enter(l,((l,k),ty,NONE),H')

		val y1 = Symbol.freshPrefix "alloc1"
		val y2 = Symbol.freshPrefix "alloc1"
		val y3 = Symbol.freshPrefix "alloc1"

		val ds = d1 @ d2 @ [Lmalloc(l,(lty,kl)),
				    (Assign ((y1,k), (Label l,k))),
				    (Memcpy ((l,k), (y2,k), (Ref(WRITE,(Var y1,k),(Field (car,0),kl)),kl), (v1',kl))),
				    (Memcpy ((l,k), (y3,k), (Ref(WRITE,(Var y2,k),(Field (cdr,1),k)),k),  (v2',k)))]

	    in
		(Var y3, k, ty, ds, (U',H',P'))
	    end
	    

	  | alloc_v (Clos.RecordVal (ss,env), (U,H,P,T,Tt))  = 
	    let 
                val l  = Symbol.freshPrefix "record"
		val y0 = Symbol.freshPrefix "alloc2"

		val (ts,vs,ds,y,(U',H',P')) =  
		    List.foldr (fn(s,(ts,vs,ds,y,(U,H,P)))=> 
				  let 
				      val v  = Env.look (s,env)
				      val (v',_,_,ds',(U',H',P')) = alloc_v (v,(U,H,P,T,Tt))
				      val t   = alloc_t H' P' T (ds',v')
				      val y'  = Symbol.freshPrefix "alloc2"
				  in 
				      (t :: ts, v' :: vs, ds @ ds', y' :: y, (U',H',P'))
				  end) 
			       ([], [], [], [], (U,H,P)) ss

		val (fs,U':utmap,_)  = 
		    foldr (fn((s,t),(fs,U,i))=> 
			     let 
				 val (k,U')  = ttEnter (t,U)
			     in 
				 ((s,(t,k),i) :: fs,U',i-1)
			     end) ([],U',(List.length ss)-1) (ListPair.zip (ss,ts))

		val (p,P') = phEnter(Vector.fromList (map (fn(s) => UN) ss), P')
		val ty     = Record (fs,p)
		val (k,U') = ttEnter(ty,U')

		val (yn,ys) = ListPair.foldl (fn (((s,(t,kf),i),yi1),v,(yi,ax)) => 
						 (yi1,
						  (Memcpy ((l,k),(yi1,k),
							   (Ref(WRITE,(Var yi,k),(Field (s,i),kf)),kf),(v,kf))) :: ax))
					     (y0, [])
					     (ListPair.zip (fs,rev y),vs)

		val H'   = Env.enter(l,((l,k),ty,NONE),H')

		val ds   = ds @ [Rmalloc (l,fs), Assign ((y0,k),(Label l,k))] @ (rev ys)

	    in
		(Var yn, k, ty, ds, (U',H',P'))
	    end

	  | alloc_v (Clos.VectorVal v, (U,H,P,T,Tt)) =
	    let
		val l  = Symbol.freshPrefix "vector"
		val y0 = Symbol.freshPrefix "alloc3"

		val n  = Vector.length v

		val (t1,vs,ds,y,(U',H',P')) = 
		    Vector.foldr (fn(v,(t1,vs,ds,y,(U,H,P))) => 
				    let val (v',_,_,ds',(U',H',P')) = alloc_v (v,(U,H,P,T,Tt)) 
					val t1 = (case t1 of 
						      SOME _ => t1
						    | NONE   => SOME (alloc_t H' P' T (ds',v')))

					val y' = Symbol.freshPrefix "alloc3"
				    in 
					(t1, v' :: vs, ds' @ ds, y' :: y, (U',H',P'))
				    end) 
				 (NONE, [], [], [], (U,H,P)) v

		val vty  = valOf t1

		val (kv,U') = ttEnter(vty,U')
		val (p,P') = phEnter(Vector.fromList [VECT(0,n)], P')
		val ty   = Vector ((vty,kv),p)

		val v'  = Vector.fromList vs
		val y   = Vector.fromList (rev y)
		val (k,U')  = ttEnter(ty,U')
		val H'      = Env.enter(l,((l,k),ty,NONE),H)

		val (yn,ys) = Vector.foldri (fn (i,yi1,(yi,ax)) => 
						(yi1,(Memcpy ((l,k),(yi1,k),
							      (Ref(WRITE,(Var yi,k),(IntNum i,kv)),kv),
							      (Vector.sub(v',i),kv))) :: ax)) 
					    (y0,[]) y

		val ds  = ds @ [Vmalloc (l,(vty,kv),Vector.length v'),
				Assign((y0,k), (Label l,k))] @ ys

	    in
		(Var yn, k, ty, ds, (U',H',P'))
	    end

	  | alloc_v (Clos.Rsel _, (U,H,P,T,Tt)) = raise TransformError
	  | alloc_v (Clos.Vref _, (U,H,P,T,Tt)) = raise TransformError

	and  alloc_e (Clos.App (v,vs), (U,H,P,T,Tt)) =
	     let
		 val (v',k,_,ds,(U',H',P')) = alloc_v (v,(U,H,P,T,Tt))

		 val (ds',vs',ks,(U',H',P')) = 
		     foldr (fn(v,(ds,vs,ks,(U,H,P))) => 
			      let
				  val (v',k,_,ds',(U',H',P')) = alloc_v (v,(U,H,P,T,Tt))
			      in (ds @ ds', v' :: vs, k :: ks, (U',H',P')) end) 
			   (ds,[],[],(U',H',P'))  vs
		     
		 val ex = App ((v',k),ListPair.zip(vs',ks))
	     in
		 (if List.null ds' then ex else Let (ds', ex), (U',H',P'))
	     end
	     
	   | alloc_e (Clos.If0(v,e1,e2), (U,H,P,T,Tt)) = 
	     let 

		 val (v',k,_,ds,(U',H',P')) = alloc_v (v,(U,H,P,T,Tt))
		 val (e1',(U',H',P'))       = alloc_e (e1,(U',H',P',T,Tt))
		 val (e2',(U',H',P'))       = alloc_e (e2,(U',H',P',T,Tt))
					      
		 val ex = If0((v',k),e1',e2')
	     in
		 (if List.null ds then ex else Let (ds, ex), (U',H',P'))
	     end
	    
	  | alloc_e (Clos.Halt(t,v), (U,H,P,T,Tt))  = 
	    let
		val (v',k,_,ds,(U',H',P')) = alloc_v (v,(U,H,P,T,Tt))
		val (t',(U',P')) = A (t,(U',P'))
		val ex = Halt (t', (v',k))
	    in
		(if List.null ds then ex else Let (ds, ex), (U',H',P'))
	    end

	  | alloc_e (Clos.Let(s,Clos.Vref (Clos.Var x,i),e), (U,H,P,T,Tt))  =   
	    let
		val (i',_,_,ds',(U',H',P')) = alloc_v (i,(U,H,P,T,Tt))
		val t            = Env.look (x,T)
		val k            = Env.look (x,Tt)
		val (tv,kv)       = case t of 
					(Vector ((t,k),_)) => (t,k)
				      | (Exists(_,((Vector ((t,k),_)),_))) => (t,k)
				      | _        => raise TransformError  
							  before (print "alloc_e: Vref (1)\n";
								  print "t = "; alloc_pptyp t; print "\n")
		val T'              = Env.enter (s,tv,T)
		val Tt'             = Env.enter (s,kv,Tt)
		val (e',(U',H',P')) = alloc_e (e,(U',H',P',T',Tt'))
	    in
		(Let (ds' @ [Assign((s,kv),(Ref(READ,(Var x,k),(i',kv)),kv))],e'), (U',H',P'))
	    end

	  | alloc_e (Clos.Let(s,Clos.Vref _,e), _)  =   
	    raise TransformError before print "alloc_e: Vref (2)\n"
	    
	  | alloc_e (Clos.Let(s,Clos.Rsel (Clos.Var x,f),e), (U,H,P,T,Tt))  =
	    let
		val t         = Env.look(x,T)
		val k         = Env.look(x,Tt)

		val (ft,kf,fi)   = case t of (Record (fs,_)) => 
					     (case List.find (fn(s,_,_) => s=f) fs of
						  SOME (s,(t,k),i) => (t,k,i)
						| NONE       => raise TransformError before print "alloc_e: Rsel (1)\n")
					     
					   |  (Exists(_,( (Record (fs,_)),_))) => 
					     (case List.find (fn(s,_,_) => s=f) fs of
						  SOME (s,(t,k),i) => (t,k,i)
						| NONE       => raise TransformError  before print "alloc_e: Rsel (2)\n")
					     
					   | _           => raise TransformError before print "alloc_e: Rsel (3)\n"

		val T'              = Env.enter (s,ft,T)
		val Tt'             = Env.enter (s,kf,Tt)
		val (e',(U',H',P')) = alloc_e (e,(U,H,P,T',Tt'))

	    in
		(Let ([Assign((s,kf),(Ref(READ,(Var x,k),(Field (f,fi),kf)),kf))], e'), (U',H',P'))
	    end

	  | alloc_e (Clos.Let(s,Clos.Rsel _,e),_)  =   
	    raise TransformError before print "alloc_e: Rsel (4)\n"

	  | alloc_e (Clos.Let(s,v,e), (U,H,P,T,Tt)) =
	    let
		val (v',k,t,ds,(U',H',P')) = alloc_v (v,(U,H,P,T,Tt))
		val T'                    = Env.enter (s,t,T)
		val Tt'                   = Env.enter (s,k,Tt)
		val (e',(U',H',P'))       = alloc_e (e,(U',H',P',T',Tt'))
	    in
		(Let (ds @ [Assign ((s,k),(v',k))],e'), (U',H',P'))
	    end

          | alloc_e (Clos.Unpack (a,x,v,e), (U,H,P,T,Tt)) =
            let
                val (v',k,t,ds,(U',H',P'))   = alloc_v (v,(U,H,P,T,Tt))
                val (t,k)      = (case t of 
				      (Exists(a,(t,k))) => (t,k)
				    | _ => raise TransformError before print "alloc_e: Unpack\n")
                val T'              = Env.enter (x,t,T)
                val Tt'             = Env.enter (x,k,Tt)
                val (e',(U',H',P')) = alloc_e (e,(U',H',P',T',Tt'))
            in
                (Let (ds @ [Unpack (a,(x,k),(v',k))], e'), (U',H',P'))
            end

	fun alloc_cp_decl V (d :: r, ax) = 
	    (case d of
		 Assign ((x,k1), (Prim (v,ts,vs), k2)) => 
		 let
		     val d' = Assign((x,k1),(Prim (alloc_cp_val V v,ts,map (alloc_cp_val V) vs), k2))
		 in
		     alloc_cp_decl V (r, d' :: ax)
		 end

	       | Assign ((x,_), (Label _,_))   => 
		 alloc_cp_decl V (r, d :: ax)

	       | Assign ((x,_), (v,_))      => 
		 let
		     val V' = Env.enter (x,alloc_cp_val V v,V)
		 in 
		     alloc_cp_decl V' (r, ax) 
		 end

	       | Unpack (a,(x,k1),(v,k2)) => 
		 let 
		     val d'  = Unpack(a,(x,k1),(alloc_cp_val V v, k2))

		     val V' = case Env.find (x,V) of
				   SOME _ => Env.remove(x,V)
				 | NONE   => V
		 in 
		     alloc_cp_decl V' (r, d' :: ax)
		 end

	       | Memcpy  ((l,k1),(x,k2),(dst,k3),(src,k4)) => 
		 let
		     val d'  = Memcpy((l,k1),(x,k2),(alloc_cp_val V dst,k3),(alloc_cp_val V src,k4))

		     val V' = case Env.find (x,V) of
				   SOME _ => Env.remove(x,V)
				 | NONE   => V
		 in 
		     alloc_cp_decl V' (r, d' :: ax)
		 end
            )

	  | alloc_cp_decl V ([], ax) = (V, rev ax)


	and alloc_cp_val V v  = 
	    (case v of 
		 Var  x             => (case Env.find (x,V) of SOME v => v | NONE   => v)
	       | Pack (t,(v,kv),t') => Pack (t, (alloc_cp_val V v,kv), t')
	       | Tapp (v,t)         => Tapp (alloc_cp_val V v, t)
	       | Prim (v,ts,vs)     => Prim (alloc_cp_val V v, ts, map (alloc_cp_val V) vs)
	       | Ref (r,(x,kx),(v,kv))  => Ref (r,(alloc_cp_val V x,kx),(alloc_cp_val V v,kv))
	       | _ => v)
			       
	fun alloc_cp_term V e = 
	    (case e of 
		 App ((v,k),vs)      => App ((alloc_cp_val V v,k), 
					     map (fn(v,k) => (alloc_cp_val V v,k)) vs)
	       | If0 ((v,k),e1,e2)   => let val e1' = alloc_cp_term V e1
					    val e2' = alloc_cp_term V e2
					in
					    If0 ((alloc_cp_val V v,k), e1', e2')
					end
	       | Halt (t,(v,k))      => Halt (t, (alloc_cp_val V v,k))
	       | Let (d,e)           => let val (V',d') = alloc_cp_decl V (d,[])
					    val ex = alloc_cp_term V' e
					in 
					    if List.null d' then ex else Let (d',ex)
					end)
	    
	fun alloc_b ((l,a,xs,e),(U,H: heap,P)) =
	    let
		val _    = print ("alloc_b:  l = " ^ (Symbol.name l) ^ "\n")

		val (xs': (symbol * (typ * ttag)) list,
		     (U',P'))  =  foldr (fn((s,t),(xs,(U,P))) => 
					       let val (t',(U',P')) = A (t,(U,P))
						   val (k,U') = ttEnter(t',U')
					       in
						   ((s,(t',k)) :: xs,(U',P')) 
					       end) ([],(U,P)) xs

		val (T,Tt) =  foldl (fn((s,(t,k)),(T,Tt)) => (Env.enter(s,t,T),Env.enter(s,k,Tt))) 
				    (#env typebase,Env.empty) xs'
		val (e',(U',H',P'))  =  alloc_e (e,(U',H,P',T,Tt))

		val ty         = (Arrow(a, map (fn(s,(t,k)) => (t,k)) xs', Unit))
		val (k,U')     = ttEnter(ty,U')

		val e''        = alloc_cp_term Env.empty e'
		val b          = (a,xs',e'')
		val H' = Env.enter(l,((l,k),ty,SOME b),H')
	    in
		(U',H',P')
	    end

	val hcount = ref 0

	fun alloc_p ((s,t,h,e): Clos.program) = 
	    let

		val ll = map (fn((l,_,_,_)) => l) (Env.listItems h)
		val (U,H,P)  = foldl (fn(l,(U,H,P)) => 
					let 
					    val _ = print ("hcount: " ^ (Int.toString (!hcount)) ^ "\n")
					    val _ = (hcount := (!hcount) + 1)
					    val b = Env.look(l,h)
					in
					    alloc_b (b,(U,H,P))
					end)
				     (#utmap typebase,Env.empty,#phimap typebase) ll

		val T               = (#env typebase)
		val (e',(U',H',P')) = alloc_e (e,(U,H,P,T,Env.empty))

		val e'' = alloc_cp_term Env.empty e'

		val (t',(U',P')) = A (t,(U',P'))
	    in
		(s,t',ttMap U',H',P',e'')
	    end

    in 
	(A,alloc_e,alloc_p,typebase)
    end
end
