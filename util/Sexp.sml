
(* 
 * Sexp.sml
 *
 * S-expression datatype and parsing/pretty-printing routines.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

signature SEXP =
sig
    type symbol = Symbol.symbol
    type pos     = int
    type name    = {name: symbol, pos: pos}

    exception Error

    datatype lit   = INT  of int 
		   | REAL of real
		   | STR  of string
		   | SYM  of symbol

    datatype sexp =
	     Lit   of lit
	   | Seq   of sexp list
	   | Meta  of sexp 


    datatype token =
	     STK_LIT_INT  of int     (* integer literal *)
	   | STK_LIT_REAL of real    (* real literal *)
	   | STK_LIT_STR  of string  (* string literal *)
	   | STK_LIT_SYM  of string  (* symbolic literal *)
	   | STK_LPAR		   (* `(' *)
	   | STK_RPAR		   (* `)' *)
	   | STK_LANGLE		   (* `<' *)
	   | STK_RANGLE		   (* `>' *)


    val $ : string -> symbol

    val assq: symbol -> sexp -> sexp option

    val lex: char list -> token list
			 
    val parse: token list -> sexp

    val unparse: sexp -> string

    val pp: sexp -> string

    val makename   : symbol * pos -> name

    val ureal       : real -> sexp
    val uint        : int -> sexp
    val uname       : name -> sexp
    val urvector    : real Array.array -> sexp

    val preal       : sexp -> real
    val pint        : sexp -> int
    val pname       : sexp -> name
    val prvector    : sexp -> real Array.array


end

structure Sexp :> SEXP = 
struct

open Char

exception Error

type symbol = Symbol.symbol

val $ = Symbol.symbol

type pos     = int
type name    = {name: symbol, pos: pos}

datatype lit   = INT  of int 
	       | REAL of real
	       | STR  of string
	       | SYM  of symbol

datatype sexp =
	 Lit   of lit
       | Seq   of sexp list
       | Meta  of sexp 

datatype token =
	  STK_LIT_INT  of int     (* integer literal *)
	| STK_LIT_REAL of real    (* real literal *)
	| STK_LIT_STR  of string  (* string literal *)
	| STK_LIT_SYM  of string  (* symbolic literal *)
	| STK_LPAR			(* `(' *)
	| STK_RPAR			(* `)' *)
	| STK_LANGLE		(* `<' *)
	| STK_RANGLE		(* `>' *)


fun token2str x =
    (case x of  
	STK_LIT_INT i    =>  "STK_LIT_INT "   ^ (Int.toString i)   
      | STK_LIT_REAL r   =>  "STK_LIT_REAL "  ^ (Real.toString r)
      | STK_LIT_STR s    =>  "STK_LIT_STR "   ^ s
      | STK_LIT_SYM s    =>  "STK_LIT_SYM "   ^ s
      | STK_LPAR         =>  "STK_LPAR"
      | STK_RPAR	 =>  "STK_RPAR"	
      | STK_LANGLE	 =>  "STK_LANGLE"	 
      | STK_RANGLE	 =>  "STK_RANGLE")


fun assq' sym ((x as Seq ((Lit (SYM sym')) :: _)) :: xs) =
    if sym=sym' then SOME x else assq' sym xs
  | assq' _  [] = NONE
  | assq' _  _  = raise Error before print "error: assq'\n"

fun assq sym  (Seq xs) = assq' sym xs
  | assq _    _        = raise Error before print "error: assq\n"
		    
fun pp'  sexp =
    let 
        fun ppTerm x =
	    let 
		open Pretty 
	    in 
		case x of (Lit (INT i))    =>  (text (Int.toString i))
			| (Lit (REAL r))   =>  (text (Real.toString r))
			| (Lit (STR s))    =>  (text ("\"" ^ s ^ "\""))
			| (Lit (SYM s))    =>  (text (Symbol.name s))
			| (Meta b)         => block {l=angle L,r=angle R} 2 (ppTerm b)
			| (Seq (bs))       => let val b =  list 2 (ppTerm,break) bs
					      in 
						  block {l=paren L,r=paren R} 2 (group b)
					      end
	    end
	
    in
	ppTerm sexp
    end

fun pp sexp = Pretty.toString (Pretty.format 60 (pp' sexp))

fun isSym c =
    (case c of 
	 #";"   => false
       | #"("   => false
       | #")"   => false
       | #"<"   => false
       | #">"   => false
       | #"\""  => false
       | _      => isGraph c)

fun munch ts []				= rev ts
  | munch ts (#" "  :: cs)	        = munch ts cs
  | munch ts (#";"  :: cs)              = comment ts cs
  | munch ts (#"\t" :: cs)		= munch ts cs
  | munch ts (#"\n" :: cs)		= munch ts cs
  | munch ts (#"("  :: cs)		= munch (STK_LPAR::ts) cs
  | munch ts (#")"  :: cs)		= munch (STK_RPAR::ts) cs
  | munch ts (#"<"  :: cs)		= munch (STK_LANGLE::ts) cs
  | munch ts (#">"  :: cs)		= munch (STK_RANGLE::ts) cs
  | munch ts (#"\"" :: cs)	        = strlit ts cs
  | munch ts (#"-"  :: c :: cs)         = if (isDigit c) then numlit ts (#"-" :: c :: cs) 
					  else raise Error before print "error: munch isDigit\n"
  | munch ts (cs as c::_)             	= if (isDigit c) then numlit ts cs
					  else if (isSym c) 
					  then symlit ts cs else raise Error 
								       before print "error: munch isSym\n"

and comment ts (#"\n"::cs)              = munch ts cs
  | comment ts (c::cs)                  = comment ts cs
  | comment ts ([])                     = List.rev ts


and symlit  ts cs			= symlit' ts [] cs
and symlit' ts l cs                   = 
    let 
	val symlit'' = fn (l, cs) => (let val s = implode (rev l)
					    val t = STK_LIT_SYM s
					in munch (t :: ts) cs end)
    in
	case cs of
	    c::cs => if (isSym c) then symlit' ts (c::l) cs
		     else symlit'' (l, c::cs)
	  | nil   => symlit'' (l, cs)
    end


and numlit ts cs			    = intlit ts [] cs
and intlit ts l ((c as #"-")::cs)     = intlit ts (c::l) cs
  | intlit ts l ((c as #".")::cs)     = reallit ts (c::l) cs
  | intlit ts l cs                    =
    let
	val intlit' = fn (l, cs) => (let val s = implode (List.rev l) 
					   val n = Int.fromString s
				       in  (case n of 
						SOME n => munch ((STK_LIT_INT n) :: ts) cs
					      | NONE => raise Error before print "error: intlit\n")
				       end)
    in
	case cs of 
	    c::cs => if (isDigit c) then intlit ts (c::l) cs
		     else intlit' (l, c::cs)
	  | nil   => intlit' (l, cs)
    end
    
and reallit ts l ((c as #"e")::cs)    = reallitsign ts (c::l) cs
  | reallit ts l ((c as #"E")::cs)    = reallitsign ts (c::l) cs
  | reallit ts l cs                   = 
    let 
	val reallit' = fn (l, cs) => (let val s = implode (List.rev l)
					    val n = Real.fromString s
					in  (case n of 
						 SOME n => munch ((STK_LIT_REAL n) :: ts) cs
					       | NONE => raise Error before print "error: reallit\n")
					end)
    in
	case cs of 
	    c::cs => if (isDigit c) then reallit ts (c::l) cs
		     else reallit' (l, c::cs)
	  | nil   => reallit' (l, cs)
    end
    
and reallitsign ts l ((c as #"+")::cs)      = reallitexp ts (c::l) cs
  | reallitsign ts l ((c as #"-")::cs)      = reallitexp ts (c::l) cs
  | reallitsign ts l cs                     = reallitexp ts l cs
						
and reallitexp ts l cs                      = 
    let
	val reallitexp' = fn (l, cs) => (let val s = implode (List.rev l)
					       val n = Real.fromString s
					   in  (case n of 
						    SOME n => munch ((STK_LIT_REAL n) :: ts) cs
						  | NONE => raise Error before print "error: reallitexp'\n")
					   end)
    in
	case cs of 
	    c::cs => if (isDigit c) then reallitexp ts (c::l) cs 
		     else  reallitexp' (l, c::cs)
	  | nil   => reallitexp' (l, cs)
    end

and strlit ts (cs)                     = strlit' ts [] cs
and strlit' ts ss (#"\"" :: cs)        = munch ((STK_LIT_STR (String.implode (rev ss))) :: ts) cs
  | strlit' ts ss (#"\\" :: c :: cs)   = let val (c, cs)  = strspecial [] (c :: cs)
					  in strlit' ts (c :: ss) cs end
  | strlit' ts ss (c :: cs)           = strlit' ts (c :: ss) cs
  | strlit' ts ss _                   = raise Error before print "error: strlit'\n"

and strspecial [] (#"n" :: cs)              = (#"\n", cs)
  | strspecial [] (#"t" :: cs)              = (#"\t", cs)
  | strspecial [] (#"\\" :: cs)             = (#"\\", cs)
  | strspecial ds (c :: cs)                 =  
    if (isDigit c) then strspecial (c :: ds) cs
    else (let val i = Int.fromString (String.implode (rev ds))
	  in 
	      case i of SOME i => (Char.chr i, cs)
		      | NONE   => raise Error before print "error: strspecial isDigit\n"
	  end)
  | strspecial _ _  = raise Error before print "error: strspecial _\n"
    
    
fun lex s = munch  [] s

fun  equal (STK_LIT_STR s1,  STK_LIT_STR s2)   = s1=s2
   | equal (STK_LIT_SYM s1,  STK_LIT_SYM s2)   = s1=s2
   | equal (STK_LIT_REAL r1, STK_LIT_REAL r2)  = Real.compare (r1,r2) = General.EQUAL
   | equal (STK_LIT_INT i1,  STK_LIT_INT i2)   = i1=i2
   | equal (STK_LPAR,        STK_LPAR)         = true
   | equal (STK_RPAR,        STK_RPAR)         = true
   | equal (STK_LANGLE,      STK_LANGLE)       = true
   | equal (STK_RANGLE,      STK_RANGLE)       = true
   | equal (_, _)                              = false

fun match(t', t :: ts)       =  if equal (t,t') then ts else  raise Error before print "error: match\n"
  | match(t', [])   	     =  raise Error before print "error: match empty\n"

fun sexp (STK_LPAR::ts)      = let val (n,ts,l') = sub 1 [] ts
                               in
                                   if List.null ts 
				   then Seq  l' 
				   else raise Error 
					      before (print "error: sexp\n";
						      print "ts = "; 
						      app (fn(x) => print ((token2str x) ^ "  ")) ts)
                               end
  | sexp _                  = raise Error before print "sexp _\n"

and sub n l ((STK_LIT_INT i)::ts)    = sub n ((Lit (INT i))::l) ts
  | sub n l ((STK_LIT_REAL r)::ts)   = sub n ((Lit (REAL r))::l) ts
  | sub n l ((STK_LIT_STR s)::ts)    = sub n ((Lit (STR  s))::l) ts
  | sub n l ((STK_LIT_SYM s)::ts)    = sub n ((Lit (SYM (Symbol.symbol s)))::l) ts
  | sub n l (STK_LPAR::ts)           = let val (n',ts,e) = sub (n+1)  [] ts 
					   val _      = if n=n' then () else 
							raise Error 
							      before (print "sub: paren mismatch; next 10 tokens: ";
								      app (fn x => print (" " ^ (token2str x) ^ " "))
								      (if Int.> (length ts, 10)
								       then List.take (ts,10)
								       else List.take (ts, length ts)))

				      in 
					   sub n ((Seq e)::l) ts 
				      end
  | sub n l (STK_RPAR::ts)           = (n-1, ts,  rev l)
  | sub n l (ts as STK_LANGLE::_)    = let val (ts,m) = meta ts in sub n (m::l) ts end
  | sub n l (STK_RANGLE::ts)         = (n-1, ts,  rev l)
  | sub n l []                       = (n, [], rev l)


and meta (STK_LANGLE::ts)         = let val (_,ts,e) = sub 0 [] ts in (ts, Meta (Seq e)) end
  | meta _                        = raise Error before print "error: meta _ \n"


fun parse ts  = sexp ts 

fun unparse (Lit (INT i))    = "Lit (INT "^(Int.toString i)^")"
  | unparse (Lit (REAL r))   = "Lit (REAL "^(Real.toString r)^")"
  | unparse (Lit (STR s))    = "Lit (STR (\""^ s ^"\"))"
  | unparse (Lit (SYM s))    = "Lit (SYM (Symbol.symbol \""^(Symbol.name s)^"\"))"
  | unparse (Meta b)         = "Meta (" ^ (unparse b) ^ ")"
  | unparse (Seq (bs))       = (let val items = map unparse bs
				in
				    "Seq ["^(String.concatWith "," items) ^"]"
				end)
fun makename  (n,p) = {name=n, pos=p}

val uname    = fn {name=n,pos=_}: name   => Lit (SYM n)
val uint     = fn i: int    => Lit (INT i)
val ureal    = fn r: real   => Lit (REAL r)
val urvector = fn rv: real Array.array   => 
		  (let val rs = Array.foldr (fn (r,rs) => (Lit (REAL r)) :: rs) [] rv
		   in Meta (Seq rs) end)

val pname    =  fn (p) =>
		   (case p of
			Lit (SYM s)  => makename (s, 0)
		      | _            => raise Error before (print ("error in pname: p = "); pp p; print "\n"))

val pint      =  fn (p) =>
		    (case p of
			 Lit (INT i)  => i
		       | _            => raise Error before (print ("error in pint: p = "); pp p; print "\n"))

val preal     =  fn (p) =>
		    (case p of 
			 Lit (REAL r) => r
		       | _            => raise Error before (print ("error in preal: p = "); pp p; print "\n"))

val prvector  =  fn (p) => 
		    (case p of
			 Meta (Seq rs)  => Array.fromList (map preal rs)
		       | _              => raise Error before (print ("error in prvector: p = "); pp p; print "\n"))
		    

end
