
(* 
 * Semant.sml
 *
 * System F polymorphic closure conversion type checking.
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

val pack_count  = ref 0
val unpack_count  = ref 0
val record_count = ref 0
		    
val app_count = ref 0
val halt_count = ref 0
val let_count = ref 0

fun vall pred v  = Vector.foldl (fn(v,ax) => (pred v) andalso ax) true v

fun Tt D t = 
    case t of  Pair t            => Tt D t
	     | Record (ss,env)   => List.all (fn(s) => Tt D (Env.look(s,env))) ss
	     | Vector t          => Tt D t
	     | Arrow  (a,ts,t)   => (let val D' = D @ a
				   in 
				       (List.all (fn(t) => Tt D' t) ts) andalso (Tt D' t)
				   end)
	     | Tvar s            => List.exists (fn(s') => s=s') D
	     | Exists (s,t)      => Tt (s :: D) t
	     | _                 => true

fun Vt H D E B (NilVal)          = Nil
  | Vt H D E B (UnitVal)         = Unit
  | Vt H D E B (Var s)           = (case Env.find (s,E) of
					  NONE       => (#t (Env.look(s,B)))
					| SOME {s,t} => t)
  | Vt H D E B (BoolVal b)       = Bool
  | Vt H D E B (IntNum i)        = Int
  | Vt H D E B (RealNum r)       = Real
  | Vt H D E B (PairVal (v1,v2)) = 
    let val t1 = Vt H D E B v1
	val t2 = Vt H D E B v2
	val _  = case t2 of Nil     => ()
			  | Pair t  => if  unify(t1,t) then () 
				       else raise SemantError 
						  before print "clos error: PairVal (1)\n" 
			  | _       => raise SemantError
					     before print "clos error: PairVal (2)\n" 
    in Pair t1 end

  | Vt H D E B (RecordVal (ss, v))  = 
    let
	val _ = (record_count := (!record_count) + 1)

	val env = foldr (fn(s,ax) => 
			   let val v = Env.look (s,v) 
			   in 
			       Env.enter(s,(Vt H D E B v),ax) 
			   end) Env.empty ss
    in 
	Record (ss,env)
    end

  | Vt H D E B (VectorVal v)     = 
    let val t' = Vt H D E B (Vector.sub (v,0))
	val _  = if vall (fn(t) => unify(t,t')) (Vector.map (Vt H D E B) v)
		 then () else raise SemantError
    				    before print "clos error: VectorVal (1)\n" 
    in 
	Vector t'
    end
				
  | Vt H D E B (Vref (v,i))  = 
    let val t2 = Vt H D E B v
	val t1 = Vt H D E B i
    in 
	case (t1,t2) of (Int, Vector et) => et
		      | _        => raise SemantError
					  before print "clos error: Vref (1)\n" 
    end
				
  | Vt H D E B (Rsel  (v,s)) = 
    let val t = Vt H D E B v
    in 
	case t of Record (ss,v) => 
		  (case Env.find(s,v) of
		       SOME t     => t
		     | NONE       => raise SemantError
					   before print "clos error: Rsel (1)\n")
		| _             => raise SemantError
					 before print "clos error: Rsel (2)\n"
    end


  | Vt H D E B (Tapp (v,t'))     = 
    let	
	val _ = if Tt D t' then () else raise SemantError
					      before print "clos error: Tapp (1)\n"
	
	val (a,b,ts)  = 
	    case Vt H D E B v of 
		Arrow  (a :: b, ts, Unit) => (a,b,ts)
	      | _    => raise SemantError
    			      before print "clos error: Tapp (2)\n" 
			      
	val tenv = Env.enter (a, t', Env.empty)
    in 
	Arrow (b, map (fn t => tsubst tenv t) ts, Unit)
    end

  | Vt H D E B (x as Prim (v as Var _,ats,vs))  = 
    let 
	val _ = if List.all (Tt D) ats then () else raise SemantError
							  before print "clos error: Prim (1)\n"
	
	val (a,ts,t)  = 
	    (case Vt H D E B v of 
		 Arrow  (a,ts,t) => (a,ts,t)
	       | _    => raise SemantError
    			       before print "clos error: Prim (2)\n" )
			      
	val tenv = ListPair.foldl (fn(a,t,E) => Env.enter (a,t,E)) Env.empty (a,ats)

	val ts  = map (tsubst tenv) ts
	val ts' = map ((tsubst tenv) o (Vt H D E B)) vs

	val t'  = tsubst tenv t

	fun ttyp (Arrow(_,_,t)) = ttyp t
	  | ttyp t = t
    in
	if ListPair.all unify (ts,ts') 
	then ttyp t'
	else raise SemantError
		   before (print "clos error: Prim (4)\n";
			   print ("x = ");  clos_ppval x; print "\n";
			   print ("ts = "); app (fn(t) => (print ("\t * "); 
							   clos_pptyp t; print "\n")) ts;
			   print ("ts' = "); app (fn(t) => (print ("\t * "); 
							    clos_pptyp t; print "\n")) ts')
				   
    end

  | Vt H D E B (Prim _) = raise SemantError  before print "clos error: Prim (5)\n"


  | Vt H D E B (Pack (t,v,t'))   = 
    let
	val _ = (pack_count := (!pack_count) + 1)
	val _ = if Tt D t then () else raise SemantError
				       before print "clos error: Pack(1)\n"

	val (a,t') = 
	    case t' of Exists (a,t') => (a,t')
		     | _             => raise SemantError
					      before print "clos error: Pack (3)\n" 
					      
	val tenv   = Env.enter (a,t,Env.empty)
    in 
	if unify (Vt H D E B v, tsubst tenv t')
	then Exists (a,t') else raise SemantError
				      before (print "clos error: Pack (4)\n";
					      print "t = "; clos_pptyp t; print "\n";
					      print "Vt H D E B v = "; clos_pptyp (Vt H D E B v); print "\n";
					      print "tsubst tenv t' = "; clos_pptyp (tsubst tenv t'); print "\n")
    end

  | Vt H D E B (Label x) = 
    let 
	val (l,a,xs,e) = Env.look(x,H)
	val E' = foldl (fn((s,t),T) => (Env.enter(s,{s=s,t=t},T))) B xs
	val D  = a
    in
	Arrow(a, map (fn(s,t) => t) xs, Unit)
    end

    

and Et H D E B (App (v,vs))      = 
    let
	val _ = (app_count := (!app_count) + 1)

	val t  = Vt H D E B v 
 	val ts = 
	    case t of Arrow ([], ts, Unit) => ts
		    | _ => raise SemantError
				 before print "clos error: App (1)\n" 
				 
	val ts' = map (Vt H D E B) vs

    in
	if ListPair.all unify (ts,ts') then () 
	else raise SemantError
		   before (print "clos error: App (2)\n" ;
			   print ("v = ");  clos_ppval v; print "\n";
			   print ("vs = ");  app (fn(v) => (print "\t * "; clos_ppval v; print "\n")) vs;
			   print ("ts = ");  app (fn(t) => (print "\t * "; clos_pptyp t; print "\n")) ts;
			   print ("ts' = ");  app (fn(t) => (print "\t * "; clos_pptyp t; print "\n")) ts';
			   print "\n")
    end
			       
  | Et H D E B (If0 (v,e1,e2))   = 
    let val t = Vt H D E B v
    in 
	if unify (t, Bool) then () 
	else raise SemantError before print "clos error: If0 (1)\n";
	Et H D E B e1;
	Et H D E B e2
    end

  | Et H D E B (Halt (t,v))       = 
    let
	val _ = (halt_count := (!halt_count) + 1)
    in 
	if unify (Vt H D E B v, t) then () 
	else raise SemantError before print "clos error: Halt (1)\n" 
    end

  | Et H D E B (Let (s,v,e))   = 
    let
	val _ = (let_count := (!let_count) + 1)
	val t = Vt H D E B v 
    in 
	Et H D (Env.enter (s,{s=s,t=t},E)) B e
    end

  | Et H D E B (Unpack (a,x,v,e)) = 
    let
	val _ = (unpack_count := (!unpack_count) + 1)
	val (a,t) = case Vt H D E B v of
			Exists (a,t) => (a,t) 
		      | _            => raise SemantError
					      before print "clos error: Unpack (1)\n" 
    in 
	Et H (a :: D) (Env.enter (x,{s=x,t=t},E)) B e
    end


fun Bt H B ((l,a,xs,e): block)  = 
    let
	val D  = a
	val _  = if List.all (fn(s,t) => Tt D t) xs 
		 then () else raise SemantError 
				    before print "clos error: Bt\n"
	val E' = foldl (fn((s,t),T) => Env.enter(s,{s=s,t=t},T))  B xs
    in
	Et H D E' B  e
    end

				  
fun Pt B (s,t,h: heap,e) = 
    let
	val ll = Env.foldl (fn((l,_,_,_),ax) => l :: ax) [] h
	val _  = app (fn(l) => 
                         let 
                             val b: block = Env.look(l,h)
		         in Bt h B b end) ll

	val _ = print ("unpack count: "  ^ (Int.toString (!unpack_count)) ^ "\n")
	val _ = print ("record count: " ^ (Int.toString (!record_count)) ^ "\n")
	val _ = print ("let count: " ^ (Int.toString (!let_count)) ^ "\n")
	val _ = print ("app count: " ^ (Int.toString (!app_count)) ^ "\n")
	val _ = print ("halt count: " ^ (Int.toString (!halt_count)) ^ "\n")

    in 
	Et h [] B B e
    end


val (clos_Vt,clos_Et,clos_Bt,clos_Pt) = (Vt,Et,Bt,Pt)
				  

end

