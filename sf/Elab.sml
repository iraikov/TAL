
(* 
 * Elab.sml
 *
 * System F type elaboration.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Elab : ELAB =
struct
    open PrettyPrint

    exception ElabError

    fun ctor (Tcon (Tabs (a,u), v)) = 
	let
	    val T = ListPair.foldl (fn(a, v, T) => Env.enter (a, v, T))  Env.empty (a, v)
	in
	    tsubst T u
	end
      | ctor t = t

    fun fcmp (f1,f2) = 
	(case (f1,f2) of 
	     (Field(s1,t1), Field(s2,t2)) => (Symbol.compare (s1,s2))
	   | (_,_)                        =>  raise ElabError)

    fun unify (Tcon(Pair, [u]), Nil)               = true
      | unify (Nil, Tcon (Pair, [v]))              = true
      | unify (Tcon(Pair, [u]), Tcon(Pair, [v]))   = unify (u, v) 
      | unify (Tcon(Vector,[u]), Tcon(Vector,[v])) = unify (u,v)
      | unify (Tcon(Record, u), Tcon(Record, v))   = 
	(ListPair.all (fn(Field (x,u),Field (y,v)) => 
			 (x=y andalso unify (u,v))
		       | (_,_) => false)     
		      (ListMergeSort.uniqueSort fcmp u, ListMergeSort.uniqueSort fcmp v))
      | unify (Tcon(Arrow, [u, v]), Tcon(Arrow, [u',v']))      = (unify (u, u')) andalso (unify (v, v'))
      | unify (t1 as Tcon(Tabs _, _), t2 as Tcon (Tabs _, _))  =    unify (ctor t1, ctor t2)
      | unify (t1 as Tcon(Tabs _, _), t2)                      =    unify (ctor t1, t2) 
      | unify (t1, t2 as Tcon(Tabs _, _))                      =    unify (t1, ctor t2)
      | unify (Tabs (a,u), Tabs (a',u'))                       = 
	let 
	    val T = ListPair.foldl (fn(a', a, T) => Env.enter (a', Tvar a, T))  Env.empty (a', a)
	in
	    unify (u, tsubst  T  u') 
	end
      | unify (Tvar a, Tvar b) = if a = b then true else false
      | unify (t1, t2)  = if t1 = t2 then true else false


    fun esubst K (Abs  (s,t,e))           =  Abs (s,tsubst K t,esubst K e)

      | esubst K (Tlet (tbs,e))           =  let val K' = ListPair.foldl (fn(s,t,K) => Env.enter(s,t,K))
									 K (ListPair.unzip tbs)
					     in
						 esubst K' e
					     end

      | esubst K (Tapp (Tlam (v,e), ts))  = let val K' = ListPair.foldl (fn(s,t,K) => Env.enter(s,t,K)) K (v,ts)
					    in
						esubst K' e
					    end

      | esubst K (Tlam (v,e))             = let val v' =  List.tabulate (length v, fn(x) => Symbol.freshPrefix("t"))
						val K' =  Type.compose (v,v',K)
					    in 
						Tlam(v', esubst K' e)
					    end

      | esubst K (If (e1,e2,e3))          = If (esubst K e1, esubst K e2, esubst K e3)
      | esubst K (App (e1,e2))            = App (esubst K e1, esubst K e2)

      | esubst K (Rcon fs)                = Rcon (map (fn(s,e) => (s, esubst K e)) fs)
      | esubst K (Rsel (e,s))             = Rsel (esubst K e,s)

      | esubst K (Vcon es)                = Vcon (map (fn(e) => esubst K e) es)
      | esubst K (Vref (e,i))             = Vref (esubst K e, esubst K i)

      | esubst K e                        = e


    and elab K (Const c) = Texp (Type.constant c, Const c)
      | elab K (Var x)   = 
	(if Env.mem(x,#env typebase)
	 then Tprim(Env.look(x,K),Var x)
	 else Texp ((Env.look (x,K) 
		     handle Env.Unbound _ => 
			    raise ElabError
				  before print ("elab: Var x: x = " ^ (Symbol.name x) ^ "\n")), Var x))
			   
      | elab K (If(e1,e2,e3)) =
	let
	    val (t1,e1') = case elab K e1 of
			       Texp(t1,e1) => (t1,Texp(t1,e1))
			     | Tprim(t1,e1) => (t1,Tprim(t1,e1))
			     | _            => raise ElabError

	    val (t2,e2') = case elab K e2 of
			       Texp(t2,e2) => (t2,Texp(t2,e2))
			     | Tprim(t2,e2) => (t2,Tprim(t2,e2))
			     | _            => raise ElabError

	    val (t3,e3') = case elab K e3 of 
			       Texp(t3,e3) => (t3,Texp(t3,e3))
			     | Tprim(t3,e3) => (t3,Tprim(t3,e3))
			     | _            => raise ElabError
	in
	    if (not (unify (t1, Bool)) orelse 
	       (not (unify (t2, t3))))
	    then raise ElabError 

	    else Texp (t2, If(e1',e2',e3'))
	end
      | elab K (Abs (x,t1,e)) =
	let
	    val  (t2,e') = case elab (Env.enter (x,t1,K)) e of 
			       Texp(t2,e')  => (t2,Texp(t2,e'))
			     | Tprim(t2,e') => (t2,Tprim(t2,e'))
			     | _            => raise ElabError
	in
	    Texp (Tcon (Arrow, [t1,t2]), Abs(x,t1,e'))
	end
      | elab K (Let1 ((x,t,e1),e2)) =
	let
	    val (t1,e1') = case elab K e1 of 
                               Texp(t1,e1)  => (t1,Texp(t1,e1))
			     | Tprim(t1,e1) => (t1,Tprim(t1,e1))
			     | _            => raise ElabError
								     
	    val (t2,e2') = case elab (Env.enter (x,t,K)) e2 of 
			       Texp(t2,e')  => (t2,Texp(t2,e'))
			     | Tprim(t2,e') => (t2,Tprim(t2,e'))
			     | _            => raise ElabError
	in
	    if (not (unify (t,t1))) then raise ElabError
	    else Texp(t2,Let1((x,t,e1'),e2'))
	end
      | elab K (App (e1,e2)) =
	let
	    val (t1,e1,p) = case elab K e1 of Texp(t1,e1)  => (t1,Texp(t1,e1),false)
					    | Tprim(t1,e1) => (t1,Tprim(t1,e1),true)
					    | _            => raise ElabError
	    val (t2,e2) = case elab K e2 of Texp(t2,e2) => (t2,Texp(t2,e2))
					    | Tprim(t2,e2) => (t2,Tprim(t2,e2))
					    | _           => raise ElabError
	in
	    case t1 of Tcon(Arrow, [t3,t4]) => 
		       if (unify (t3, t2)) 
		       then (if p 
			     then Tprim (t4, App(e1,e2))
			     else Texp (t4, App(e1,e2)))
		       else raise ElabError before (print "elab: App (e1,e2) [1]: \n"; 
						    print ("e1 = "); ppexp e1; print "\n";
						    print ("e2 = "); ppexp e2; print "\n";
						    print ("t3 = "); pptyp t3; print "\n";
						    print ("t2 = "); pptyp t2; print "\n")
		     | _ => raise ElabError before (print ("elab: App (e1,e2) [2]: \n"); 
						    print ("t1 = "); pptyp t1; print  "\n";
						    print ("t2 = "); pptyp t2; print "\n")
	end

      | elab K (Tlam (a, e)) = 
	let val (t, e') = case elab K e of 
			      Texp(t,e')  => (t,Texp(t,e'))
			    | Tprim(t,e') => (t,Tprim(t,e'))
			    | _           => raise ElabError
	in 
	    Texp (Tabs (a,t), Tlam (a,e'))
	end

      | elab K (ex as Tapp (Var x, v: typ list)) =
	let
	    val (t,p) = case elab K (Var x) of 
			    Texp(t,_)  => (t,false)
			  | Tprim(t,_) => (t,true)
			  | _          => raise ElabError
	in
	    case t of Tabs(a,u) =>
		      let val K  = ListPair.foldl (fn(s,t,K) => Env.enter(s,t,K)) Env.empty (a,v)
			  val t' = tsubst K u
		      in if p 
			 then Tprim (t', Tbox (x, ListPair.zip (a,v), t))
			 else Texp  (t', Tbox (x, ListPair.zip (a,v), t))
		      end
		    | _     => raise ElabError 
	end

      | elab K (e as Tapp (u, v)) = elab K (esubst Env.empty e)

      | elab K (Tlet (tbs, e)) =
	let
	    val K'  = ListPair.foldl (fn(s,t,K) => Env.enter(s,t,K)) Env.empty (ListPair.unzip tbs)
	    val e1  = esubst K' e
	in
	    elab K e1
	end
	    
      | elab K (e as Tbox (s, tbs, t)) = 
	let val K = ListPair.foldl (fn(s,t,K) => Env.enter(s,t,K)) Env.empty (ListPair.unzip tbs)
	in 
	    if (Env.mem(s,#env typebase))
	    then Tprim (tsubst K t, e) 
	    else Texp (tsubst K t, e) 
	end

      | elab K (Rcon fs) = 
	let  
	    val flds = map (fn (s,e) => (s, case elab K e of 
						Texp (t,e) => (t,e,false)
					      | Tprim (t,e) => (t,e,true)
					      | _ => raise ElabError)) fs
	    fun fldty (s,(t,e,p)) = Field(s,t)
	    fun fldexp (s,(t,e,false)) = (s,Texp(t,e))
	      | fldexp (s,(t,e,true)) = (s,Tprim(t,e))
	in
	    Texp (Tcon (Record, ListMergeSort.uniqueSort fcmp (map fldty flds)),
		  Rcon (map fldexp flds))
	end
      | elab K (Rsel (e,s)) = 
	let
	    fun flook s f = (case f of Field (s', _) => 
				       if s=s' then true else false
				     | _             => 
				       raise ElabError before print "elab: error in rsel flook\n" )

	    val (t2, e2) = case elab K e of 
			       Texp(t2,e2) => (t2,Texp(t2,e2))
			     | Tprim(t2,e2) => (t2,Tprim(t2,e2))
			     | _           => raise ElabError
	in
	    case ctor t2 of 
		Tcon (Record, flds) => 
		(case List.find (flook s) flds of 
		     SOME (Field (s,t)) => 
		     Texp (t, Rsel (e2,s))
		   | _ => 
		     raise ElabError before print "elab: error in rsel\n")
	      | _                   => 
		raise ElabError before print "elab: error in rsel\n"
        end

      | elab K (Vref (e,i)) = 
	let
	    val  (t2, e2) = case elab K e of Texp(t2,e2)  => (t2,Texp(t2,e2))
					   | Tprim(t2,e2) => (t2,Tprim(t2,e2))
					   | _            => raise ElabError
	    val  (t3, i2) = case elab K i of Texp(t3,i2)  => (t3,Texp(t3,i2))
					   | Tprim(t3,i2) => (t3,Tprim(t3,i2))
					   | _            => raise ElabError
	in
	    if (ctor t3) = Int 
	    then 
		(case ctor t2 of 
		     Tcon (Vector, [t]) => Texp (t,Vref(e2,i2))
		   | _                   => raise ElabError before print ("elab: error in vref\n"))
	    else raise ElabError before print "elab: error in vref\n"
        end

      | elab K (Vcon (e :: es)) = 
	let  
	    val (ts,es') = ListPair.unzip (map (fn (e) => case elab K e of 
							      Texp (t,e) => (t,Texp(t,e))
							    | Tprim (t,e) => (t,Tprim(t,e))
							    | _          => raise ElabError) (e :: es))
	    val _  = foldl (fn (t,st) => if unify (t,st) then st 
					 else raise ElabError before print ("elab: error in vcon\n")) 
			   (hd ts) (tl ts)
	in
	    Texp (Tcon (Vector, [hd ts]), Vcon es')
	end

      | elab K (Vcon [])       =  raise ElabError before print ("elab: error in vcon\n")

      | elab K (e as Texp _)  =  e
      | elab K (e as Tprim _) =  e
			
	     

    fun elab_program K (p, ev, ty, e) =
	let

    	    val (K',ev')  = foldr (fn((s,t,e), (K',ev)) => 
				    let
					val  (t1,e1) = case elab K e of 
							   Texp(t1,e1)  => (t1,Texp(t1,e1))
							 | Tprim(t1,e1) => (t1,Tprim(t1,e1))
							 | _           => raise ElabError
					val _ = if (not (unify (t1, t))) 
						then raise ElabError 
                                                     before (print ("elab: error in program\n");
							     print "t1 = "; pptyp t1; print "\n";
							     print "t = "; pptyp t; print "\n")
						else ()
				    in
					(Env.enter (s,t,K'), (s,t,e1)::ev)
				    end)
				 (K,[]) ev

	    val (t,e)    = case elab K' e of Texp(t,e)  => (t,Texp(t,e))
					   | Tprim(t,e) => (t,Tprim(t,e))
					   | _          => raise ElabError
								 
	    val _        = if (not (unify(t,ty))) 
                           then raise ElabError 
                                before (print ("elab: error in program\n");
					print "ty = "; pptyp ty; print "\n";
					print "t = "; pptyp t; print "\n")
			   else ()
								
	in
	    (t, (p, ev', ty, e))
	end

end
