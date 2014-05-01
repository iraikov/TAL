
(* 
 * PrettyPrint.sml
 *
 * System F allocation pretty printing.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure PrettyPrint :> PRETTYPRINT =
struct

open Syntax
		 
fun ppSymbol (s) = Pretty.text (Symbol.name s)
		   

fun ppVector (ppElem, ppSep) v  = Vector.foldri (fn(i,v,ax) => (ppElem (i,v)) :: ax) [] v

fun ppTerm' x = let open Pretty in 
		    block {l=paren L, r=paren R} 0 x
		end

fun ppVelem ppSep n (i,v) = 
    (if i=n then ppVal  v  else Pretty.cons (ppVal v, ppSep()))
    
and ppField (x,v) = 
    let open Pretty in 
	binop 2 (ppSymbol x, text "=", ppVal v)
    end
    
and ppArgument (x,(ty,_)) = 
    let open Pretty in 
	binop 2 (ppSymbol x, text ":", alloc_pptyp' ty)
    end


and ppFieldTy (x,ty,i) = 
    let open Pretty in 
	binop 2 (ppSymbol x, text ":", alloc_pptyp' ty)
    end
    
    
and ppVal v =
    let open Pretty 
    in
	case v of 
	    (NilVal)             => text "#nil"
	  | (UnitVal)            => text "#unit"
	  | (Var x)              => ppSymbol x
	  | (IntNum i)           => text (Int.toString i)
	  | (RealNum r)          => text (Real.toString r)
	  | (BoolVal true)       => text "#true"
	  | (BoolVal false)      => text "#false"

	  | (Pack((t,_),(v,_),(t',_)))   => 
	    ppTerm' (connect (group (connect (text "pack", group (list 2 (fn x => x, comma) 
								      [alloc_pptyp' t,ppVal v]))),
			     group (connect (text "as", alloc_pptyp' t'))))

          | (Tapp(v,t))      => 
	    ppTerm' (connect (ppVal v, block {l=angle L, r=angle R} 2 (alloc_pptyp' t)))

          | (Prim(v1,ts,vs))  => 
	    ppTerm' (connect (group (connect (group (connect (text "prim", ppVal v1)), 
					     if (List.null ts) then empty()
					     else block {l=angle L, r=angle R} 2 
							(list 2 (fn(t,k) => alloc_pptyp' t,comma) ts))),
			     block {l=paren L, r=paren R} 2 
				   (list 2 (ppVal,comma) vs)))

	  | (Label l)        => 
	    ppTerm' (connect(text "label", ppSymbol l))

	  | (Field (s,i))    => 
	    ppTerm' (group (connect (text "Field", connect(ppSymbol s,text ("(" ^ (Int.toString i) ^ ")")))))

	  | (Ref (m,(x,_),(v,_)))    => 
	    ppTerm' (let
			 val v = case m of READ   => group (connect (text "#", ppVal v))
					 | WRITE  => group (connect (text "!", ppVal v))
		     in 
			 group (connect(v, block {l=paren L, r=paren R} 2 (ppVal x)))
		     end)
	    
	end

and ppDecl d =
    let open Pretty 
    in
	case d of
	    Assign ((s,_),(v,k))   => 
	    ppTerm' (binop 2 (ppSymbol s, text "=", ppVal v))


	  | Unpack (a,(x,kx),(v,k))  => 
	    ppTerm' (connect (text "unpack",
			     binop 2 (list 2 (fn x => x,comma) [ppSymbol a,ppSymbol x], 
				      text "=", ppVal v)))
	    
	  | Lmalloc (l,(t,_))   => 
	    ppTerm' (binop 2 (ppSymbol l, text "=",
			      group (connect (text "malloc",
					      block {l=paren L, r=paren R} 2
						    (alloc_pptyp' t)))))
	    
	  | Vmalloc (l,(t,_),i)   =>  
	    ppTerm' (binop 2 (ppSymbol l, text "=",
			      group (connect (text "malloc",
					      block {l=paren L, r=paren R} 2
						    (list 2 (fn x => x,comma)
							  [alloc_pptyp' t,text (Int.toString i)])))))


	  | Rmalloc (l,fs)    => 
	    ppTerm' (binop 2 (ppSymbol l, text "=",
			      group (connect (text "malloc",
					      block {l=paren L, r=paren R} 2
						    (list 2 (fn(s,(t,k),i)=>ppFieldTy(s,t,i),comma) fs)))))
	    
	  | Memcpy((l,kl),(x,kx),(d,kd),(s,ks))   => 
	    ppTerm' (binop 2 (ppSymbol x, text "=", 
			      group (connect(text "memcpy", 
					     block {l=paren L, r=paren R} 2
						   (list 2 (ppVal,comma) [d,s])))))
	    


    end

    and ppTerm x = 
	let open Pretty 	
	in 
	    case x of 
		(App ((v,k),vs))   => 
		ppTerm' (connect (ppVal v, (list 2 (fn(v,k) => ppVal v,break) vs)))

	      | (Halt (ty,(v,k)))     => 
		ppTerm' (connect (text "halt", 
				 group (connect (block {l=angle L, r=angle R} 2 (alloc_pptyp' ty),
						 ppVal v))))
				    
	      |  (If0  ((v,k),e1,e2))  => 
		 ppTerm' (ifthen {i=text "if",t=text "then",e=text "else"} 2 
				(ppVal v, ppTerm e1, ppTerm e2))
											     
	      | (Let  (ds,e))    => 
		ppTerm' (letblk {l=text "let", i=text "in", e=text "end"} 2  
			       (list 2 (ppDecl,break) ds, ppTerm e))
		 
	end

    and ppBlock (l,(a,xs,e))     =  
	let open Pretty 
	in 
	    connect (binop 2 (group (connect (group (connect (text "block", ppSymbol l)),
					      group (connect (if (List.null a)
							      then empty()
							      else block {l=angle L, r=angle R} 2 
									 (list 2 (ppSymbol,comma) a),
							      block {l=paren L, r=paren R} 2 
								    (list 2 (ppArgument,comma) xs))))),
			      text "=>", ppTerm e),
		     breakWith "\n")
 	end


val alloc_ppval'   = ppVal

val alloc_ppdecl'  = ppDecl

val alloc_ppblock' = ppBlock

val alloc_pp'      = ppTerm


fun alloc_ppblock (l,b) = Pretty.printf TextIO.stdOut (Pretty.format 60 (alloc_ppblock' (l,b)))

fun alloc_ppdecl d  = Pretty.printf TextIO.stdOut (Pretty.format 60 (alloc_ppdecl' d))

fun alloc_ppval v   = Pretty.printf TextIO.stdOut (Pretty.format 60 (alloc_ppval' v))

fun alloc_pp p      = Pretty.printf TextIO.stdOut (Pretty.format 60 (alloc_pp' p))


fun alloc_ppWithStream strm e = Pretty.printf strm (Pretty.format 60 (alloc_pp' e))
fun alloc_ppblockWithStream strm (l,b) = Pretty.printf strm (Pretty.format 60 (alloc_ppblock' (l,b)))


end
