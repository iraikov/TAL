
(* 
 * PrettyPrint.sml
 *
 * System F CPS pretty printing.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure PrettyPrint : PRETTYPRINT =
struct

open Syntax

fun ppSymbol (x) = (Pretty.text (Symbol.name x))

fun ppVector (ppElem, ppSep) v  = Vector.foldri (fn(i,v,ax) => (ppElem (i,v)) :: ax) [] v

fun ppTerm x = let open Pretty in 
		   block {l=paren L, r=paren R} 0 x
	       end

fun ppVelem ppSep n (i,v) = 
    (if i=n then ppVal  v  else Pretty.cons (ppVal v, ppSep()))
    
and ppField (x,v) = 
    let open Pretty in 
	binop 2 (ppSymbol x, text "=", ppVal v)
    end
    
and ppArgument (x,ty) = 
    let open Pretty in 
	binop 2 (ppSymbol x, text ":", cps_pptyp' ty)
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

	  | (PairVal (v1,v2))    => block {l=bracket L,r=bracket R} 2
					  (list 2 (ppVal,comma) [v1,v2])

	  | (RecordVal (ss,env)) => 
	    ppTerm (block {l=brace L, r=brace R} 2
			  (list 2 (ppField,comma) (map (fn(x) => (x,Env.look(x,env))) ss)))
				   
	  | (VectorVal vs)   => 
	    ppTerm (let val n = Vector.length vs
		    in
			block {l=bracket L,r=bracket R} 2 (text "...")
		    end)

	  | (Vref (v,i))  => 
	    ppTerm (connect (ppVal v, (block {l=bracket L, r=bracket R} 2 (ppVal i))))
			     
	  | (Rsel (v,f))  => 
	    ppTerm (binop 2 (ppVal v, text ".", ppSymbol f))
	    
	  | (Proc (ss,xs,e))   => 
	    ppTerm (binop 2 (group (connect (text "proc",
					     group (connect (if (List.null ss)
							     then empty()
							     else block {l=angle L, r=angle R} 2 
									(list 2 (ppSymbol,comma) ss),
							     block {l=paren L, r=paren R} 2 
								   (list 2 (ppArgument,comma) xs))))),
			     text "=>", ppExp e))
	    
	  | (Prim(v1,ts,v2))   => 
	    ppTerm (group (connect (group (connect (group (connect (text "prim", ppVal v1)), 
						    if (List.null ts) then empty()
						    else block {l=angle L, r=angle R} 2 
							       (list 2 (cps_pptyp',comma) ts))),
				    ppVal v2)))
    end

    and ppExp x = 
	let open Pretty 	
	in 
	    case x of 
		(App (v,ts,vs))   => 
		ppTerm (connect (group (connect (ppVal v,
						 if (List.null ts) then empty()
						 else block {l=angle L, r=angle R} 2 
							    (list 2 (cps_pptyp',comma) ts))),
				 (list 2 (ppVal,break) vs)))

	      | (Halt (ty,v))     => 
		ppTerm (connect (text "halt", 
				 group (connect (block {l=angle L, r=angle R} 2 (cps_pptyp' ty),
						 ppVal v))))
				    
	      |  (If0  (v,e1,e2))  => 
		 ppTerm (ifthen {i=text "if",t=text "then",e=text "else"} 2 
				(ppVal v, ppExp e1, ppExp e2))
					      
	      | (Let  (x,v,e))    => 
		ppTerm (letblk {l=text "let", i=text "in", e=text "end"} 2  
			       (binop 2 (ppSymbol x, text "=", ppVal v), ppExp e))


	end


val cps_ppval'   = ppVal

val cps_pp'      = ppExp

fun cps_ppval v  = Pretty.printf TextIO.stdOut (Pretty.format 60 (cps_ppval' v))

fun cps_pp e     = Pretty.printf TextIO.stdOut (Pretty.format 60 (cps_pp' e))

fun cps_ppWithStream strm e = Pretty.printf strm (Pretty.format 60 (cps_pp' e))

end
