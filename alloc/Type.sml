
(* 
 * Type.sml
 *
 * System F allocation type definitions.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Type :> TYPE =
struct

type symbol  = Symbol.symbol

datatype phi = UN | IN | VECT of int * int

type ptag = int
type ttag = int

datatype typ = Nil
	     | Unit 
	     | Bool 
	     | Int 
	     | Real 
	     | Pair    of (typ * ttag) * ptag
	     | Vector  of (typ * ttag) * ptag
	     | Record  of (symbol * (typ * ttag) * int) list * ptag
	     | Arrow   of (symbol list) * ((typ * ttag) list) * typ
	     | Tvar    of symbol
	     | Exists  of symbol * (typ * ttag)

type ttyp = typ * ttag

type typenv = typ Env.env
type ttagenv = ttag Env.env

fun fcmp ((s1,t1,_),(s2,t2,_)) = (Symbol.compare (s1,s2))

					      
fun compose (a,v,T1: typenv) = 
    let 
	fun compose1 (a,v,T1) =
	    let 
		val a' = Env.find (a,T1)
	    in
		case a' of 
		    SOME a' => 
		    let
			val T2  = Env.enter (a, (Tvar v),Env.empty)
			val T1' = Env.map (fn(t) => tsubst T2 t) T1
			val T1' = Env.enter (a, (Tvar v), T1')
		    in
			Env.enter (v,a',T1')
		    end
		  | NONE    => Env.enter (a, (Tvar v),T1)
	    end
    in
	ListPair.foldl compose1 T1 (a,v)
    end
    
and  tsubst  T (t as  (Tvar a)) = 
     (case Env.find (a, T) of 
	  SOME t => t
	| NONE   => t)

   | tsubst  T ((Arrow (a,ts,u))) =
     let
	 val ss  = List.tabulate (length a, fn(x) => Symbol.freshPrefix "allocty")
	 val T'  = compose (a,ss,T)
     in
	 (Arrow (ss, map (fn(t,k) => (tsubst T' t,k)) ts, tsubst T' u))
     end

   | tsubst T ((Pair ((t,k),p))) = 
     (Pair ((tsubst T t,k),p))

   | tsubst T ((Vector ((t,k),p))) = 
     (Vector ((tsubst T t,k), p))

   | tsubst T ((Record (fs,p))) = 
     let
	 val fs' = map (fn(s,(t,k),i) => (s,(tsubst T t,k),i)) fs 
     in
	 (Record (fs',p))
     end
     

   | tsubst T ((Exists (a,(t,k)))) = 
     let 
	 val s = Symbol.freshPrefix "allocty" 
	 val T' = compose ([a],[s],T)
     in 
	 (Exists(s,(tsubst T' t,k)))
     end

   | tsubst T t                   = t


fun unify (Nil, Nil)           = true
  | unify (Unit, Unit)         = true
  | unify (Bool, Bool)         = true
  | unify (Int, Int)           = true
  | unify (Real, Real)         = true 

  | unify ((Pair ((u,_),up)), (Pair ((v,_),vp))) = unify (u,v)
  | unify ((Vector ((u,_),up)), (Vector ((v,_),vp))) = unify (u,v) 

  | unify ((Record (ufs,up)), (Record (vfs,vp))) = 
    ListPair.all (fn((s',(t',_),i'),(s'',(t'',_),i'')) => unify(t',t'')) (ufs,vfs)

  | unify ((Arrow (a,u,t)), (Arrow (a',u',t'))) = 
    (if (List.length a) = (List.length a') 
     then (let val ss  = List.tabulate (length a, fn(x) => Symbol.freshPrefix "allocty")
	       val T  =  compose (a,ss,Env.empty)
	       val T' =  compose (a',ss,Env.empty)
	   in 
	       (ListPair.all (fn((u,_),(u',_)) => unify (tsubst T u, tsubst T' u')) (u, u')) 
	       andalso (unify (tsubst T t, tsubst T' t'))
	   end)
     else false)

  | unify ((Tvar s), (Tvar s'))    = s=s'
  | unify ((Exists (s,(t,_))), (Exists (s',(t',_)))) = 
    let val p  =  Symbol.freshPrefix "allocty"
	val T  =  Env.enter (s,(Tvar p),Env.empty)
	val T' =  Env.enter (s',(Tvar p),Env.empty)
    in 
	unify (tsubst T t, tsubst T' t')
    end
  | unify (_, _)               = false


fun alloc_pptyp' ty =
    let 
	fun ppSymbol (s) = Pretty.text (Symbol.name s)

	fun ppPhi p =
	    let open Pretty in 
		case p of UN => text "UN"
			| IN => text "IN"
			| VECT (i,n) =>  
			  let open Pretty in
			      group (connect 
					 (text "VECT", 
					  block {l=paren L, r=paren R} 2
						(list 2 (fn i => text (Int.toString i),comma) [i,n])))
			  end
	    end

	and ppField (x,(t,_),i) = 
	    let open Pretty in 
		binop 2 (ppSymbol x, text ":", ppType t)
	    end
    
			     
	and ppType (x) = 
	    let open Pretty in 
		case x of 
		    (Nil)  => text "Nil"
		  | (Unit) => text "Unit"
		  | (Bool) => text "Bool"
		  | (Int)  => text "Int"
		  | (Real) => text "Real"
		  
		  | ((Pair ((ty,_),p))) => 
		    group (connect (text "Pair", ppType ty))

		  | ((Record (fs,p))) => 
		    group (connect (text "Record", 
				    block {l=brace L, r=brace R} 2
					  (list 2 (ppField,comma) fs)))
		    
		  | ((Vector ((ty,_),p)))   => 
		    group (connect (text "Vector", ppType ty))

		  | ((Arrow (a,xs,t)))  => 
		    binop 2 (group (connect (if (not (List.null a))
					     then block {l=angle L,r=angle R} 2
							(list 2 (ppSymbol,comma) a)
					     else empty(),
					     block {l=angle L,r=angle R} 2
						   (list 2 (fn(t,k) => ppType t,comma) xs))),
			     text "->",
			     ppType t)

		  | ((Tvar x))      => 
		    block {l=brace L, r=brace R} 2 (ppSymbol x)

		  | ((Exists(x,(t,_))))  => 
		    binop 2 (group (connect (text "E", block {l=paren L, r=paren R} 2 
							     (ppSymbol x))), 
			     text ".", ppType t)


	    end

    in
	ppType ty
    end

	
fun alloc_pptyp ty = Pretty.printf TextIO.stdOut (Pretty.format 60 (alloc_pptyp' ty))


type phislot = phi Vector.vector

structure P = IntMapFn (type key = ptag
                        val getInt = (fn(x) => x))

type phimap  = phislot P.map

val nptag = ref 1

val phNull  = 0

val phEmpty = P.empty

fun phEnter (sl,m) = 
    let val i = !nptag
	val _ = nptag := (!nptag) + 1
    in
	(i,P.enter(m,i,sl))
    end

fun phFind (i,m) = P.find (m,i)


structure BA = BitArray

open Hash

val b0    = false
val b1    = true
val bx    = BA.fromList 

val bnull       = ref (bx [])
val bnil        = ref (bx [b1])
val bunit       = ref (bx [b1, b0])
val bbool       = ref (bx [b1, b1])
val bint        = ref (bx [b1, b0, b0])
val breal       = ref (bx [b1, b0, b1])
val btvar       = ref (bx [b1, b1, b0])
val bpair       = ref (bx [b1, b1, b1])
val bvector     = ref (bx [b1, b0, b0, b0])
val brecord     = ref (bx [b1, b0, b0, b1])
val bexists     = ref (bx [b1, b0, b1, b0])
val barrow      = ref (bx [b1, b0, b1, b1])
val bchannel    = ref (bx [b1, b1, b0, b0])


exception ProjectError

    
fun project' ck (t, f, s: (Word32.word * int * (Word8.word ref))) =
    let
	fun ckfold ([], s) = s
	  | ckfold (t :: [], s) =
	    (project' ck) (t,false,s)
	  | ckfold (t :: r, s) =
	    ckfold (r, (project' ck) (t,true,s))
	    
    in
	case t of Nil            => ck (bnil,f,s)
		| Unit           => ck (bunit,f,s)
		| Bool           => ck (bbool,f,s)
		| Int            => ck (bint,f,s)
		| Real           => ck (breal,f,s)
		| Tvar _         => ck (btvar,f,s)
		| Pair ((t,_),_)     => (project' ck) (t,f,ck (bpair,true,s)) 
		| Vector ((t,_),_)   => (project' ck) (t,f,ck (bvector,true,s)) 
		| Exists(a,(t,_))    => (project' ck) (t,f,s) 
		| Record([],_)   => ck (brecord,f,s)
		| Record(fs,_)   => ckfold (map (fn(s,(t,_),_) => t) fs, (ck(brecord,true,s)))
		| Arrow(a,ts,t)  => ckfold (map (fn(t,_) => t) ts, (ck(barrow,true,s)))
    end

fun project (t) = 
    let 
	val (ck,h0) = fnv8
	val w0 = Word8.wordSize-1
	val q0 = ref (0w0: Word8.word)

	val (h,w,q) = (project' (cksum8 ck)) (t,false,(h0,w0,q0))
    in
	Int.abs (Word32.toIntX h)
    end


fun tequal (Nil, Nil)   = true
  | tequal (Unit, Unit) = true
  | tequal (Bool, Bool) = true
  | tequal (Int, Int)   = true
  | tequal (Real, Real) = true
  | tequal ((Pair ((u,_),_)), (Pair ((v,_),_))) = tequal (u,v)
  | tequal ((Vector ((u,_),_)), (Vector ((v,_),_))) = tequal (u,v)
  | tequal ((Record (u,_)), (Record (v,_))) = 
    ListPair.all (fn((su,(u,_),_),(sv,(v,_),_)) => su=sv andalso tequal (u,v)) (u,v)
  | tequal ((Arrow (_,u,up)), (Arrow (_,v,vp))) =
    (ListPair.all (fn((u,_),(v,_)) => tequal (u,v)) (u,v)) andalso tequal (up,vp)
  | tequal ((Tvar _),(Tvar _)) = true
  | tequal ((Exists (_,(u,_))), (Exists (_,(v,_)))) = tequal (u,v)
  | tequal (u, v) = false

structure H = SfhtFn (type key = typ
                      val equal = tequal
                      val getInt = project)


structure M = IntMapFn (type key = ttag
                        val getInt = (fn(x) => x))


val ttNull   = 0
val nttag    = ref 1


type tmap = (typ M.map)

type rmap = (ttag H.map)

type utmap =  rmap * tmap 

fun ttMap (h,m) = m

fun ttEmpty (cap) = (H.empty (cap, 0.0001), M.empty)

fun ttInt k = k
	     
val inil  = (!nttag before (nttag := (!nttag) + 1))
val iunit = (!nttag before (nttag := (!nttag) + 1))
val ibool = (!nttag before (nttag := (!nttag) + 1))
val iint  = (!nttag before (nttag := (!nttag) + 1))
val ireal = (!nttag before (nttag := (!nttag) + 1))
val itvar = (!nttag before (nttag := (!nttag) + 1))

val tnil   =  Nil
val tunit  =  Unit
val tbool  =  Bool
val tint   =  Int
val treal  =  Real
val ttvar  =  (Tvar (Symbol.symbol "###"))

fun ttFind (m,M) = 
    (if m=inil   then (SOME tnil)  else 
     (if m=iunit  then (SOME tunit) else
      (if m=ibool  then (SOME tbool) else
       (if m=iint   then (SOME tint)  else
	(if m=ireal  then (SOME treal) else 
         (if m=itvar  then (SOME ttvar) else M.find (M,m)))))))

fun ttEnter (t,(h,m)) = 
    (case t of 
	 Nil      => (inil,(h,m))
       | Unit     => (iunit,(h,m))
       | Bool     => (ibool,(h,m))
       | Int      => (iint,(h,m))
       | Real     => (ireal,(h,m))
       | (Tvar _) => (itvar,(h,m))
       | x    =>
	 (case H.find(t,h) of 
	      SOME i =>  (i, (h,m))
	    | NONE   =>  (let 
			      val i = !nttag
			      val _ =  nttag := (!nttag) + 1
			      val h' = H.insert (t,i,h)
			      val m' = M.enter (m,i,t)
			  in 
			      (i, (h',m'))
			  end)))

val ttAppi = M.appi

val ttFoldli = M.foldli

val ttBase = 
    let
	val m = ttEmpty 262144

	fun enter (t,i,(h,m)) = (H.insert (t,i,h), M.enter (m,i,t))

	val m = enter (tunit,iunit,m)
	val m = enter (tnil,inil,m)
	val m = enter (tbool,ibool,m)
	val m = enter (tint,iint,m)
	val m = enter (treal,ireal,m)
	val m = enter (ttvar,itvar,m)
    in
	m
    end


end
