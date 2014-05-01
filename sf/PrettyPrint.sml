
(* 
 * PrettyPrint.sml
 *
 * System F pretty printing.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure PrettyPrint : PRETTYPRINT =
struct

open Sugar

    fun ppSymbol (s) = Pretty.text (Symbol.name s)

    fun ppTerm x = let open Pretty in 
		       block {l=paren L, r=paren R} 0 x
		   end

    fun ppTval (x,ty) = 
	let open Pretty in 
	    group (connect (text "type", binop 2 (ppSymbol x, text "=", pptyp' ty)))
	end

    fun ppPval (x,ty,e) = 
	let open Pretty in 
	    (binop 2 (binop 2 (ppSymbol x, text ":", pptyp' ty), text "=", ppExp e))
	end

    and ppField (x,e) = 
	let open Pretty in 
	    (binop 2 (ppSymbol x, text "=", ppExp e))
	end

    and ppExp x =
	let open Pretty in
	    case x of 
		(Const    UNIT_CONST)          => text "#unit"
	      | (Const    NIL_CONST)           => text "#nil"
	      | (Const    (REAL_CONST r))      => text (Real.toString r)
	      | (Const    (INT_CONST i))       => text (Int.toString i)
	      | (Const    (BOOL_CONST true))   => text "#true"
	      | (Const    (BOOL_CONST false))  => text "#false"
	      | (Var      x)                   => ppSymbol x
	      | (If       (e1,e2,e3)) => 
		ppTerm (ifthen {i=text "if",t=text "then",e=text "else"} 2 
			       (ppExp e1, ppExp e2, ppExp e3))

	      | (App (e1,e2))    => 
		ppTerm (connect (ppExp e1, ppExp e2))
						  
	      | (Abs (x,ty,e))   => 
		ppTerm (binop 2 (group (connect (text "fn",
						 block {l=paren L, r=paren R} 2 
						       (binop 2 (ppSymbol x, text ":", pptyp' ty)))),
				 text "=>", ppExp e))

	      | (Let1 ((x,ty,e1),e2))   => 
		ppTerm (binop 2 (group (connect (text "let",
						 block {l=paren L, r=paren R} 2 
						       (binop 2 (binop 2 (ppSymbol x, text ":", pptyp' ty),
								 text "=", ppExp e1)))),
				 text "in", ppExp e2))
					     
	      | (Tapp (e,ts)) => 
		ppTerm (connect (group (connect (text "Tapp", ppExp e)), 
				 list 2 (pptyp',comma) ts))
						   
	      | (Tlet     (tbs, e)) => 
		ppTerm (letblk {l=text "Tlet", i=text "in", e=text "end"} 2  
			       (list 2 (ppTval,space) tbs, ppExp e))
						 
	      | (Tbox     (x,tbs,ty)) =>  
		ppTerm (connect (ppSymbol x,
				 block {l=angle L, r=angle R} 2
				       (letblk {l=text "Tbox", i=text "in", e=text "end"} 2  
					       (list 2 (ppTval,space) tbs, pptyp' ty))))

						    
	      | (Tlam     (xs,e)) => 
		ppTerm (binop 2 (connect (text "Tfn",
					  block {l=angle L, r=angle R} 2 
						(list 2 (ppSymbol,comma) xs)),
				 text "=>", ppExp e))
						    
	      | (Rcon flds) => 
		(block {l=brace L, r=brace R} 2
		       (list 2 (ppField,comma) flds))
					    
	      | (Rsel     (e,s)) => 
		ppTerm (binop 2 (ppExp e, text ".", ppSymbol s))
						    
	      | (Vcon     (es)) => 
		(block {l=bracket L, r=bracket R} 2
		       (list 2 (ppExp,comma) es))
						    
	      | (Vref     (e,i)) => 
		(connect (ppExp e, (block {l=bracket L, r=bracket R} 2 (ppExp i))))
		
	      | (Texp     (ty,e))  => ppExp e

	      | (Tprim     (ty,e)) => ppExp e

						    
	end
						  
    fun pptoplevel (Exp exp) = ppExp exp
      | pptoplevel (Program (s, ebs, ty, e))  =
	let open Pretty  in
	    letblk {l=binop 2 (connect (text "program", ppSymbol s), text ":", pptyp' ty),
		    i=text "in", e=text "end"} 2
		   (list 2 (ppPval,break) ebs,ppExp e)
	end


fun ppexp x = Pretty.printf TextIO.stdOut (Pretty.format 60 (ppExp x))

val pp' = pptoplevel

fun pp x = Pretty.printf TextIO.stdOut (Pretty.format 60 (pptoplevel x))

fun ppWithStream strm x = Pretty.printf strm (Pretty.format 60 (pp' x))


end
