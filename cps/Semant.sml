
(* 
 * Semant.sml
 *
 * System F continuation-passing style type checking.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Semant: SEMANT =
struct

open PrettyPrint

exception SemantError

fun vall pred v  = Vector.foldl (fn(v,ax) => (pred v) andalso ax) true v


fun Vt E (NilVal)          = Nil
  | Vt E (UnitVal)         = Unit
  | Vt E (Var s)           = Env.look (s,E)
  | Vt E (BoolVal b)       = Bool
  | Vt E (IntNum i)        = Int
  | Vt E (RealNum r)       = Real
  | Vt E (PairVal (v1,v2)) = 
    let val t1 = Vt E v1
	val t2 = Vt E v2
	val _  = (case t2 of Nil     => ()
			   | Pair t  => 
			     if  unify(t1,t) then () 
			     else raise SemantError
					before print "cps error: Vt E (PairVal _) (1)\n"
			   | _       => 
			     raise SemantError
				   before print "cps error: Vt E (PairVal _) (2)\n")
    in Pair t1 end

  | Vt E (RecordVal (ss, v))  = 
    let val ts = foldr (fn(s,ax) => 
			  let val v = Env.look (s,v) 
			  in (Vt E v) :: ax end) [] ss
    in 
	Record (ListPair.zip (ss,ts))
    end

  | Vt E (VectorVal v)     = 
    let val t' = Vt E (Vector.sub (v,0))
	val _  = if vall (fn(t) => unify(t,t')) (Vector.map (Vt E) v)
		 then () else raise SemantError 
				    before print "cps error: Vt E (VectorVal v)\n"
    in 
	Vector t'
    end

  | Vt E (Vref  (v,i)) = 
    let val t2 = Vt E v
	val t1 = Vt E i
    in 
	case (t1,t2) of 
	    (Int, Vector et) =>  et
	  | _                =>  raise SemantError
				       before print "cps error: Vt E (Vref _)\n"
    end

  | Vt E (Rsel  (v,s)) = 
    let val t = Vt E v
    in 
	case t of Record v => 
		  let val ft = case List.find (fn(s',_) => s=s') v of
				   SOME (s,t) => t
				 | NONE       => 
				   raise SemantError
					 before print "cps error: Vt E (Rsel _) (1)\n"
		  in 
		      ft
		  end
		| _             => raise SemantError
					 before print "cps error: Vt E (Rsel _) (2)\n"
    end
    
  | Vt E (Prim (v1,ats,v2)) = 
    let val t = Vt E v1
		
	val (a,ts,t)  = (case t of Arrow (a,ts,t) => (a,ts,t)
				 | _  => raise SemantError
					       before print "cps error: Vt E (Prim _) (1)\n")
			
	val tenv  = ListPair.foldl Env.enter Env.empty (a,ats)
		    
	val ts    = map (tsubst tenv) ts
		    
	val t2   = ((tsubst tenv) o (Vt E)) v2
    in 
	case ts of 
	    t1 :: [] => 
	    if unify(t1,t2) then tsubst tenv t
	    else raise SemantError
		       before (print "cps error: Vt E: Prim: (2)\n";
			       print ("v1 = "); cps_ppval v1; print "\n";
			       print ("v2 = "); cps_ppval v2; print "\n";
			       print ("t1 = "); cps_pptyp t1; print "\n";
			       print ("t2 = "); cps_pptyp t2; print "\n")
	  | t1 :: ts => 
	    if unify(t1,t2) 
	    then Arrow([],ts,t)
	    else raise SemantError
		       before (print "cps error: Vt E: Prim: (3)\n";
			       print ("v1 = "); cps_ppval v1; print "\n";
			       print ("v2 = "); cps_ppval v2; print "\n")
	  | []  => raise SemantError
			 before (print "cps error: Vt E: Prim: (4)\n";
				 print ("v1 = "); cps_ppval v1; print "\n";
				 print ("v2 = "); cps_ppval v2; print "\n")
    end

  | Vt E (Proc (a, xs, e)) = let val E' = foldl (fn ((s,t),E) => (Env.enter (s,t,E))) E xs 
				   val _  = Et E' e
			       in 
				   Arrow (a, map (fn((s,t)) => t) xs, Unit)
			       end
			     

		 
and Et E (x as App (v,ats,vs)) = 
    (let 
	 val t   = Vt E v

	 val (a,ts,t)  = (case t of Arrow (a,ts,t) => (a,ts,t)
				  | _              => raise SemantError
							    before print "cps error: Et E (App _) (3)\n")
							    
	 val tenv  = ListPair.foldl Env.enter Env.empty (a,ats)

	 val ts    = map (tsubst tenv) ts

	 val vts   = map ((tsubst tenv) o (Vt E)) vs

     in 
	 if ListPair.all unify (ts, vts) then () 
	 else raise SemantError
		    before (print "cps error: Et E (App _) (2)\n";
			    print ("x = "); cps_pp x; print "\n";
			    print ("v = "); cps_ppval v; print "\n";
			    print ("vs = \n");
			    app (fn(v) => (print "\t - "; cps_ppval v; print "\n")) vs;
			    print ("ts = \n");
			    app (fn(t) => (print "\t - "; cps_pptyp t; print "\n")) ts;
			    print ("vts = \n");
			    app (fn(t) => (print "\t - "; cps_pptyp t; print "\n")) vts)
     end)

  | Et E (If0 (v,e1,e2))   = let val t = Vt E v
			     in 
				 if unify (t, Bool) then () 
				 else raise SemantError before print "cps error: Et E (If0 _)\n";
				 Et E e1;
				 Et E e2
			     end


  | Et E (Let   (s,v,e)) = let val t = Vt E v 
			     in Et (Env.enter (s,t,E)) e end


  | Et E (Halt (t,v))     = if unify (Vt E v, t) then () 
			      else raise SemantError
					 before print "cps error: Et E (Halt _)\n"
					   


fun Pt E (s,t,e)  =  Et E e
 

val (cps_Vt,cps_Et,cps_Pt) = (Vt,Et,Pt)

end
