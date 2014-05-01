
(* 
 * SXML.sml
 *
 * Auxiliary functions for S-expression based XML.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

signature SXML =
sig
    type symbol  = Symbol.symbol
    type pos     = int
    type name    = {name: symbol, pos: pos}

    type sexp   = Sexp.sexp

    exception Error
    exception FcondError
    exception CondError
    exception VassqError  of (symbol * Sexp.sexp)
    exception OvassqError of (symbol * Sexp.sexp)
    exception SvassqError of (symbol * Sexp.sexp)
    exception AttrError   of Sexp.sexp
    exception KidsError   of Sexp.sexp
    exception KidError    of Sexp.sexp
    exception ValofError  of Sexp.sexp
    exception ListvalofError of Sexp.sexp
    exception PnameError  of Sexp.sexp
    exception PboolError  of Sexp.sexp
    exception PintError   of Sexp.sexp
    exception PrealError  of Sexp.sexp
    exception PrvectorError of Sexp.sexp

    val ATTR    : symbol
    val T_ATTR  : int

    val S_NONE     : sexp
    val T_S_NONE   : int

    val T_S_SOME   : int

    val someval    : sexp -> sexp
    val somelist   : sexp list -> sexp

    val TYPE_BOOL    : symbol
    val T_TYPE_BOOL   : int
    val TYPE_INT     : symbol
    val T_TYPE_INT   : int
    val TYPE_REAL    : symbol
    val T_TYPE_REAL  : int
    val TYPE_NAME    : symbol
    val T_TYPE_NAME  : int
    val TYPE_RVECTOR    : symbol
    val T_TYPE_RVECTOR  : int

    val cond    : (bool * 'a) list -> 'a
    val fcond   : (bool * (unit -> 'a)) list -> 'a

    val vassq   : symbol -> sexp -> (sexp list)
    val svassq  : symbol -> sexp -> sexp

    val ovassq  : symbol -> sexp -> (sexp list) option

    val listvalof : (sexp -> 'a) -> sexp -> ('a list) option
    val valof     : (sexp -> 'a) -> sexp -> 'a option

    val attr    : sexp -> sexp

    val vlookattr   : symbol -> sexp -> (sexp list)
    val svlookattr  : symbol -> sexp -> sexp

    val kids        : sexp -> sexp list
    val kid         : sexp -> sexp option

    val ureal       : real -> sexp
    val uint        : int -> sexp
    val ubool       : bool -> sexp
    val uname       : name -> sexp
    val urvector    : real Array.array -> sexp

    val preal       : sexp -> real
    val pint        : sexp -> int
    val pbool       : sexp -> bool
    val pname       : sexp -> name
    val prvector    : sexp -> real Array.array


end

structure Sxml :> SXML = 
struct


type symbol = Symbol.symbol

type pos     = int

type name    = {name: symbol, pos: pos}

type sexp   = Sexp.sexp

    exception Error
    exception FcondError
    exception CondError
    exception VassqError  of (symbol * Sexp.sexp)
    exception OvassqError of (symbol * Sexp.sexp)
    exception SvassqError of (symbol * Sexp.sexp)
    exception AttrError   of Sexp.sexp
    exception KidsError   of Sexp.sexp
    exception KidError    of Sexp.sexp
    exception ValofError  of Sexp.sexp
    exception ListvalofError of Sexp.sexp
    exception PnameError  of Sexp.sexp
    exception PintError   of Sexp.sexp
    exception PboolError  of Sexp.sexp
    exception PrealError  of Sexp.sexp
    exception PrvectorError of Sexp.sexp


val ATTR  as (_,T_ATTR)                   = Symbol.symbol "@"

val (S_NONE,T_S_NONE)                     = let open Sexp
						val  s as (_,t) = Symbol.symbol "type:none"
					    in 
						(Seq [Lit (SYM s)], t)
					    end

val (someval, somelist, T_S_SOME)         = let open Sexp
						val  s as (_,t) = Symbol.symbol "type:some"
					    in 
						(fn x => Seq [Lit (SYM s), x],
						 fn x => Seq ((Lit (SYM s)) :: x),
						 t)
					    end

val TYPE_BOOL as (_,T_TYPE_BOOL)          = Symbol.symbol "type:bool"
val TYPE_INT as (_,T_TYPE_INT)            = Symbol.symbol "type:int"
val TYPE_REAL as (_,T_TYPE_REAL)          = Symbol.symbol "type:real"
val TYPE_NAME as (_,T_TYPE_NAME)          = Symbol.symbol "type:name"
val TYPE_RVECTOR as (_,T_TYPE_RVECTOR)    = Symbol.symbol "type:rvector"

fun cond ((c,x) :: r)       = if c then x else cond r
  | cond nil                = raise CondError
				    
fun fcond ((c,x) :: r)      = if c then x() else fcond r
  | fcond nil               = raise FcondError

fun vassq s x = (let open Sexp in 
		     case assq s x of 
			 SOME (Seq ((Lit (SYM s)) :: v)) => v 
		       | _  => raise VassqError (s,x)
		 end)
		    
fun ovassq s x = (let open Sexp in 
		      case assq s x of 
			  SOME (Seq ((Lit (SYM s)) :: v)) => SOME v 
			| NONE => NONE
			| _    => raise OvassqError (s,x)
		  end)
		 
fun svassq s x = (let val v = vassq s x
		  in
		      case v of [v] => v
			      | _   => raise SvassqError (s,x)
		  end)

val attr = let open Sexp 
	       fun attr (Seq x)   = Seq [Lit (SYM ATTR), Seq x]
		 | attr x         = raise AttrError x
	   in
	       attr
	   end

fun vlookattr  k x = vassq k (Sexp.Seq (vassq ATTR x))
fun svlookattr k x = svassq k (Sexp.Seq (vassq ATTR x))

val kids = let open Sexp 
	       fun kids (Seq ((Lit (SYM (_,x)) :: r))) =  
		   List.filter (fn (Seq ((Lit (SYM (_,x)) :: r))) => not (x = T_ATTR)
				 | _  => true) r
		 | kids (Seq []) = raise KidsError (Seq [])
		 | kids x        = raise KidsError x
	   in kids end
    

val kid = let open Sexp 
	      fun kid (Seq ((Lit (SYM (_,x)) :: r))) =  
		  List.find (fn (Seq ((Lit (SYM (_,x)) :: r))) => not (x = T_ATTR)
			      | _  => true) r
		| kid x       = raise KidError x
	  in kid end
    

fun listvalof p x = (let open Sexp 
		     in 
			 (case x of Seq ((Lit (SYM (_,s))) :: _) => 
				    fcond [(s=T_S_NONE, fn()=> NONE),
					   (s=T_S_SOME, 
					 fn()=> SOME (map p (kids x)))]
				  | _           => raise ListvalofError x)
		     end)

fun valof p x = (let open Sexp 
		 in 
		     (case x of Seq ((Lit (SYM (_,s))) :: _) => 
				fcond [(s=T_S_NONE, fn()=> NONE),
				       (s=T_S_SOME, 
				     fn()=> case kid x of 
						SOME v  => SOME (p v)
					      | _       => raise ValofError x)]
			      | _           => raise ValofError x)
		 end)


val uname      = (let open Sexp in 
		   fn {name,pos}: name   => Seq [Lit (SYM TYPE_NAME), Lit (STR (Symbol.name name))]
		  end)

val uint       = (let open Sexp in 
		   fn i: int    => Seq [Lit (SYM TYPE_INT),  Lit (STR (Int.toString i))]
		  end)

val ubool      = (let open Sexp in 
		   fn b: bool    => Seq [Lit (SYM TYPE_BOOL),  Lit (STR (if b then "#true" else "#false"))]
		  end)

val ureal      = (let open Sexp in 
		   fn r: real   => Seq [Lit (SYM TYPE_REAL), Lit (STR (Real.toString r))]
		  end)

val urvector   = (let open Sexp in 
		   fn rv: real Array.array   => 
		      let val rs = Array.foldl (fn (r,rs) => (ureal r) :: rs) [] rv
		      in 
			  Seq ((Lit (SYM TYPE_RVECTOR)) :: rs)
		      end
		  end)

val pname      =  (let open Sexp in 
		    fn (p) =>
		       (case p of  
			    Seq [Lit (SYM (s,n)), Lit (STR x)]      => 
			    (if n=T_TYPE_NAME 
			     then makename (Symbol.symbol x, 0) 
			     else raise Error  before print ("unrecognized type predicate in pname: " ^
							     s ^ "\n"))
			  | _ => raise PnameError p)
		   end)
		   
val  pint    =  (let open Sexp in 
		  fn (p) =>
		     (case p of
			  Seq [Lit (SYM (s,n)), Lit (STR x)]      => 
			  (if n=T_TYPE_INT 
			   then  (case (Int.fromString x) of 
				      SOME i => i
				    | NONE   => 
				      raise Error  
					    before print ("pint error: Int.fromString failed: x = " ^ x ^ "\n"))
			   else   raise Error before print ("unrecognized type predicate in pint: " ^
							    s ^ "\n"))
			| _  => raise PintError p)
		 end)

val preal   =   (let open Sexp in 
		  fn (p) =>
		     (case p of
			  Seq [Lit (SYM (s,n)), Lit (STR x)]      => 
			  (if n=T_TYPE_REAL 
			   then  (case (Real.fromString x) of 
				      SOME i => i
				    | NONE   => 
				      raise Error  
					    before print ("preal error: Real.fromString failed: x = " ^ x ^ "\n"))
			   else   raise Error before print ("unrecognized type predicate in preal: " ^
							    s ^ "\n"))
			| _  => raise PrealError p)
		 end)


val prvector =  (let open Sexp in 
		  fn (p) =>
		     (case p of 
			  Seq ((Lit (SYM (s,n))) :: _) =>
			  (if n=T_TYPE_RVECTOR
			   then  Array.fromList (map preal (kids p))
			   else  raise Error before print ("unrecognized type predicate in prvector: " ^
							   s ^ "\n"))
			| _ => raise PrvectorError  p)
		 end)

val pbool   =  let open Sexp 
	       in 
		fn (p as Seq [Lit (SYM (_,x)), Lit (STR s)])      => 
		   (if x=T_TYPE_BOOL 
		    then  (case s of 
			       "#true" => true
			     | _       => false)
		    else   raise PboolError p)
		 | p              => raise PboolError p
	       end

end
