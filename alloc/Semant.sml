
(* 
 * Semant.sml
 *
 * System F allocation type checking.
 *
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Semant :> SEMANT =
struct

open PrettyPrint

exception SemantError
exception UninitializedError
exception OverwriteError

fun vall pred v  = Vector.foldl (fn(v,ax) => (pred v) andalso ax) true v


val assign_count  = ref 0
val unpack_count  = ref 0
val memcpy_count  = ref 0
val lmalloc_count = ref 0
val vmalloc_count = ref 0
val rmalloc_count = ref 0
		    
val app_count = ref 0
val halt_count = ref 0
val let_count = ref 0


fun Ht H P T (Ref (m,(x,_),(Field (f,i),_))) = 
    let 
	val xt   = Vt H P T x
	val car  = Symbol.symbol "car"
	val cdr  = Symbol.symbol "cdr"
    in
	case xt of 
	    (Record (fs,k))  => 
	    (let val p = case phFind(k,P) of
			     SOME p => p
			   | NONE => raise SemantError
					   before (print "alloc error: Ht: Ref (1)\n";
						   print ("f = " ^ (Symbol.name f) ^ "\n"))
						   
							  
		 val t = case List.find (fn(s,(t,k),i) => s=f) fs of 
			     SOME (s,(t,k),i) => t
			   | NONE   => raise SemantError
					     before print "alloc error: Ht: Ref (2)\n"
	     in
		 case Vector.sub(p,i) of
		     IN => (case m of 
				READ  => (t,NONE)
			      | WRITE => raise OverwriteError)

		   | UN => (case m of 
				READ  => raise UninitializedError 
					       before (print "alloc error: Ht: Ref (3)\n";
						       print "t = "; alloc_pptyp t; print "\n")
			      | WRITE => let 
                                             val hp       = Vector.update(p,i,IN)
				             val (hk,P')  = phEnter(hp,P)
				             val ht       = (Record(fs,hk))
					 in 
					     (t,SOME (ht, P'))
					 end)
		   | _ => raise SemantError
				before print "alloc error: Ht: Ref (4)\n"
	     end)

	  | (Pair ((t,k),p)) => 
	    (let 
		 val p = case phFind(p,P) of
			     SOME p => p
			   | NONE => raise SemantError
					   before print "alloc error: Ht: Ref (5)\n"
	     in
		 (if Symbol.equal (f,car) 
		  then (case Vector.sub(p,0) of 
			    IN => (case m of 
				       READ   => (t, NONE)
				     | WRITE  => raise OverwriteError)
			  | UN => (case m of 
				       READ   => raise UninitializedError
				     | WRITE  => 
				       let
                                           val hp = Vector.update(p,0,IN)
                                           val (hk,P') = phEnter(hp,P)
					   val ht      = (Pair ((t,k),hk))
				       in 
				           (t, SOME (ht, P'))
				       end)
			  | _ => raise SemantError
				       before print "alloc error: Ht: Ref (6)\n")

		  else (if Symbol.equal (f,cdr) 
			then  (case Vector.sub(p,1) of 
				   IN => (case m of 
					      READ   => if Vector.sub(p,0)=IN 
							then (t, NONE) else raise UninitializedError
					    | WRITE  => raise OverwriteError)
				 | UN =>
				   (case m of 
					READ   => raise UninitializedError
				      | WRITE  => 
					let
                                            val hp = Vector.update(p,1,IN)
                                            val (hk,P') = phEnter(hp,P)
					    val ht      = (Pair ((t,k),hk))
				        in 
				            (t, SOME (ht, P'))
				        end)
				 | _ => raise SemantError 
					      before print "alloc error: Ht: Ref (7)\n")
			      
			else raise SemantError before print "alloc error: Ht: Ref (8)\n"))
	     end)
	  | _  => raise SemantError before print "alloc error: Ht: Ref (9)\n"
    end
	     
  | Ht H P T (Ref (m,(x,_),(IntNum i,_))) = 
    let 
	val xt   = Vt H P T x
    in
	case xt of 
	    (Vector ((t,k),p))  => 
	    (let val p = case phFind(p,P) of
			     SOME p => p
			   | NONE => raise SemantError
					   before print "alloc error: Ht: Ref (11)\n"
	     in
		 case Vector.sub(p,0) of
		     VECT(i',n) => (case m of
					READ  => if (i < i') andalso (i'=n)
						 then (t, NONE)
						 else raise SemantError before print "alloc error: Ht: Ref (12)\n"
				      | WRITE => if (i > i') andalso (i < n)
						 then (let
							   val hp = Vector.update(p,0,VECT(i'+1,n))
							   val (hk,P') = phEnter(hp,P)
							   val ht      = (Vector ((t,k),hk))
						       in
							   (t, SOME(ht,P'))
						       end)
						 else raise OverwriteError)
		   | IN => (case m of READ => (t,NONE)
				    | WRITE => raise SemantError)

		   | _ => raise SemantError before print "alloc error: Ht: Ref (13)\n"
	     end)
	  | _ => raise SemantError before print "alloc error: Ht: Ref (14)\n"
    end
	    

  | Ht H P T (Ref (m,(x,_),(Var i,_))) = 
    let 
	val xt   = Vt H P T x
	val _  = if unify(Int,Env.look (i,T)) 
		 then () else raise SemantError before print "alloc error: Ht: Ref (16)\n"
    in
	case xt of 
	    (Vector ((t,k),p))   => 
	    (let val p = case phFind(p,P) of
			     SOME p => p
			   | NONE => raise SemantError
					   before print "alloc error: Ht: Ref (17)\n"
	     in
		 case Vector.sub(p,0) of
		     UN => (case m of 
				READ   => raise UninitializedError 
			      | WRITE  => raise SemantError before print "alloc error: Ht: Ref (18)\n")
		   | IN => (case m of 
				READ   => (t,NONE)
			      | WRITE  => raise OverwriteError)
		   | VECT(i,n) => (case m of 
				       READ   => if i=n then (t,NONE)
						 else raise UninitializedError 
				     | WRITE  => if i<n 
						 then (let
							   val hp = Vector.update(p,0,VECT(i+1,n))
							   val (hk,P') = phEnter(hp,P)
							   val ht      = (Vector ((t,k),hk))
						       in
							   (t, SOME(ht,P'))
						       end)
						 else raise OverwriteError)
	     end)


	  | _ => raise SemantError before print "alloc error: Ht: Ref (19)\n"
    end

  | Ht H P T v = raise SemantError before (print "alloc error: Ht: v = ";
					   alloc_ppval v; print "\n")
		       

and Vt H P T (NilVal)          = Nil
  | Vt H P T (UnitVal)         = Unit
  | Vt H P T (Var s)           = Env.look (s,T)
  | Vt H P T (BoolVal b)       = Bool
  | Vt H P T (IntNum i)        = Int
  | Vt H P T (RealNum r)       = Real
  | Vt H P T (Tapp (v,t'))     = 
    let val (a,b,ts)  = case Vt H P T v of 
			    (Arrow  (a :: b, ts, Unit)) => (a,b,ts)
			  | _ => raise SemantError before print "alloc error: Vt: Tapp (1)\n"
	val tenv = Env.enter (a, t', Env.empty)
    in 
	(Arrow (b, map (fn (t,k) => (tsubst tenv t,k)) ts, Unit))
    end
			       
  | Vt H P T (Prim (v as Var _,ats,vs))  = 
    let 
	val (a,ts,t)  = 
	    (case Vt H P T v of 
		 (Arrow (a,ts,t)) => (a,ts,t)
	       | _    => raise SemantError
    			       before print "alloc error: Vt: Prim (1)\n" )
			      
	val tenv = ListPair.foldl (fn(a,(t,k),E) => Env.enter (a,t,E)) Env.empty (a,ats)

	val ts' = map ((tsubst tenv) o (Vt H P T)) vs
	val ts  = map (fn(t,k) => (tsubst tenv t)) ts

	val t'  = tsubst tenv t
	
	fun ttyp ((Arrow(_,_,t))) = ttyp t
	  | ttyp t = t

    in
	if ListPair.all unify (ts,ts') 
	then ttyp t'
	else raise SemantError
		   before (print "alloc error: Vt: Prim (2)\n";
			   print "ts = "; app (fn(t) => (alloc_pptyp t; print "\n")) ts; 
			   print "ts' = "; app (fn(t) => (alloc_pptyp t; print "\n")) ts';
			   print "t' = "; alloc_pptyp t'; print "\n")
    end

  | Vt H P T (Prim _) = raise SemantError  before print "alloc error: Vt: Prim (3)\n"


  | Vt H P T (Pack ((t,_),(v,_),(t',_)))   = 
    let val (a,t'') = case t' of 
			 (Exists (a,(t',k))) => (a,t')
		       | _ => raise SemantError
				    before print "alloc error: Vt: Pack (1)\n"
	val tenv   = Env.enter (a, t, Env.empty)
    in 
	if unify (Vt H P T v, tsubst tenv t'')
	then t' else 
	raise SemantError before (print "alloc error: Vt: Pack (2)\n";
				  print "v = "; alloc_ppval v; print "\n";
				  print "Vt H T v = "; alloc_pptyp (Vt H P T v); print "\n";
				  print "\n")
    end

  | Vt H P T (Label l) = 
    let
	val ((l,k),t,_) = Env.look(l,H)
    in 
	t
    end

  | Vt H P T (x as Ref _) = (case Ht H P T x of (t,_) => t)

  | Vt H P T _  = raise SemantError before print "alloc error: Vt\n"


and Dt H P T []  = (P,T)

  | Dt H P T (Assign((s,ks),(v,kv)) :: ds) =  
    let 
	val _  = (assign_count := (!assign_count) + 1)
	val t' = Vt H P T v
    in 
	Dt H P (Env.enter(s,t',T)) ds
    end

  | Dt H P T (((Unpack (a,(x,kx),(v,kv))) :: ds)) =
    let
	val _  = (unpack_count := (!unpack_count) + 1)

	val (a,ut) = case Vt H P T v of
			 (Exists (a,(ut,_))) => (a,ut) 
		       | _            => raise SemantError before print "Dt error: Unpack\n"
    in 
	Dt H P (Env.enter(x,ut,T)) ds
    end    

  | Dt (H: heap) P T (((Lmalloc (l,ty)) :: ds))  =
    let 
	val _  = (lmalloc_count := (!lmalloc_count) + 1)
    in 
	case Env.find(l,H) of
	    SOME (_, (Pair(t,k)), _) => Dt H P T ds
	  | _ => raise SemantError before print "Dt error: Lmalloc\n"
	
    end

  | Dt H P T (((Rmalloc(l,sts)) :: ds))  = 
    let
	val _  = (rmalloc_count := (!rmalloc_count) + 1)

    in
	case Env.find(l,H) of 
	    SOME (_,(Record(fs,k)),_) => Dt H P T ds
	  | SOME _ => raise SemantError before print "Dt error: Rmalloc (1)\n"
	  | NONE => raise SemantError before (print ("Dt error: Rmalloc (2): l = " ^ (Symbol.name l) ^ " \n"))
    end

  | Dt H P T (((Vmalloc(l,vt,i)) :: ds))  = 
    let 
	val _  = (vmalloc_count := (!vmalloc_count) + 1)
    in 
	case Env.find(l,H) of 
	    SOME (_,  (Vector(t,k)), _) => Dt H P T ds
	  | _  => raise SemantError before print "Dt error: Vmalloc\n"
    end

  | Dt H P T ((ex as Memcpy((l,kl),(x,kx),(d,kd),(s,ks))) :: ds) =
    let
	val _  = (memcpy_count := (!memcpy_count) + 1)

	val dt  = Vt H P T d
	val st  = Vt H P T s
	val (xt,P') = case Ht H P T d of
			  (_,SOME (t,P)) => (t,P)
			| (_,NONE) => raise SemantError before print "Dt error: Memcpy\n"
    in 
	if unify(st,dt) 
	then Dt H P' (Env.enter(x,xt,T)) ds
	else raise SemantError before (print "alloc error: Dt: Memcpy\n";
				       print "ex = "; alloc_ppdecl ex; print "\n";
				       print "xt = "; alloc_pptyp xt; print "\n")
    end




and Et H P T (App ((v,k),vs))  = 
    let
	val _  = (app_count := (!app_count) + 1)

	val t'  = Vt H P T v 
	val ts = case t' of  (Arrow ([], ts,  Unit)) => map (fn(t,k) => t) ts
			  | _ => raise SemantError 
				       before print "alloc error: Et: App (2)\n"
	val ts' = map (fn(v,k) => (Vt H P T)  v) vs
    in
	if ListPair.all unify (ts,ts') then () 
	else raise SemantError  before 
		   (print "alloc error: Et: App (3)\n";
		    print ("ts = ");  app (fn(t) => (print "\t - "; alloc_pptyp t; print "\n")) ts;
		    print ("ts' = ");  app (fn(t) => (print "\t - "; alloc_pptyp t; print "\n")) ts')

    end

  | Et H P T (If0 ((v,k),e1,e2)) = 
    let
	val t = Vt H P T v
    in 
	if unify (t,  Bool) then () 
	else raise SemantError
		   before print "alloc error: Et: If0\n";
	Et H P T e1;
	Et H P T e2
    end
				 
  | Et H P T (Halt (t,(v,k)))   = 
    let
	val _  = (halt_count := (!halt_count) + 1)
    in
	if unify (Vt H P T v, t) then () 
	else raise SemantError  before print "alloc error: Et: Halt (1)\n"
    end

  | Et H P T (Let (ds,t))       = 
    let
	val _  = (let_count := (!let_count) + 1)
	val (P',T') = (Dt H P T ds)
    in
	Et H P' T' t
    end


fun Bt H P T ((a,xs,e): block) =
    let
	val T = foldl (fn((s,(t,_)),T) => Env.enter(s,t,T)) T xs
    in
	Et H P T e
    end

fun Pt T (s,t,m,h: heap,p: phimap,e) = 
    let
	val ll = Env.foldl (fn((l,_,SOME _),ax) => l :: ax | (_,ax) => ax) [] h
	val _  = app (fn((l,k)) => case Env.look(l,h) of 
				       (_,_,SOME b) => 	(Bt h p T b)
				     | _ => raise SemantError) ll

	val _ = print ("assign count: "  ^ (Int.toString (!assign_count)) ^ "\n")
	val _ = print ("unpack count: "  ^ (Int.toString (!unpack_count)) ^ "\n")
	val _ = print ("memcpy count: "  ^ (Int.toString (!memcpy_count)) ^ "\n")
	val _ = print ("lmalloc count: " ^ (Int.toString (!lmalloc_count)) ^ "\n")
	val _ = print ("vmalloc count: " ^ (Int.toString (!vmalloc_count)) ^ "\n")
	val _ = print ("rmalloc count: " ^ (Int.toString (!rmalloc_count)) ^ "\n")
	val _ = print ("let count: " ^ (Int.toString (!let_count)) ^ "\n")
	val _ = print ("app count: " ^ (Int.toString (!app_count)) ^ "\n")
	val _ = print ("halt count: " ^ (Int.toString (!halt_count)) ^ "\n")
    in 
	Et h p T e
    end

val (alloc_Dt,alloc_Vt,alloc_Et,alloc_Bt,alloc_Pt) = (Dt,Vt,Et,Bt,Pt)

end

