
(* 
 * Main.sml
 *
 * System F interpreter main structure.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

structure Main: MAIN =
struct
    open TextIO
    open SF
    open Value
    open Eval

    val VERSION = "0.1"
		  
    exception Exit of OS.Process.status 

    fun valueToString (UnitVal)        = "unit"
      | valueToString (BoolVal false)  = "false"
      | valueToString (BoolVal true)   = "true"
      | valueToString (IntNum n)       = Int.toString n
      | valueToString (RealNum n)      = Real.toString n
      | valueToString (PairVal (l,r))  = String.concat ["<", valueToString l, ", ", valueToString r, ">"]
      | valueToString (VectorVal v)    = 
	let 
	    val vs = rev (Vector.foldl (fn (v,vs)  => v :: vs) [] v)
	in "{" ^ (String.concat (map (fn(v) => " " ^ (valueToString v) ^ " ") vs)) ^ "}" end
      | valueToString (RecordVal (ss,rd)) = 
	let 
	    val flds = ListPair.map (fn(s,v) => String.concat [" ", (Symbol.name s), " : ",
							       (valueToString v), " "]) 
				    (ss, Env.listItems rd)
	in
	    "{" ^ (String.concat flds)  ^ "}"
	end
      | valueToString (NilVal)      =  "nil"
      | valueToString (Op _)	    =  "fn"
      | valueToString (Proc _)	    =  "fn"
      | valueToString (RecProc _)   =  "fn"


    fun printVal (v,t) = (print (valueToString (v) ^ " : "); (pptyp t))


    fun exitError (prog, msg) = 
	let 
	    val _ = TextIO.output(TextIO.stdErr, prog ^ ": " ^ msg ^ "\n")
	in 
	    raise Exit OS.Process.failure 
	end
	    
    fun exitHelp prog = 
	let 
	    val _ = TextIO.output(TextIO.stdOut, (Options.usage prog) ^ "\n")
	in 
	    raise Exit OS.Process.success 
	end
	    
    fun exitVersion () = 
	let 
	    val _ = TextIO.output (TextIO.stdOut,  " version " ^ VERSION ^ "\n")
	in 
	    raise Exit OS.Process.success 
	end

    fun evalFromString s =
	let
	    val sexp   = Sexp.parse (Sexp.lex (String.explode s))

	    val top    = fromSXML sexp

	    val top    = (case top of 
	 		      Exp' tree     => Exp (transform tree)
			    | Program' p    => Program (transform_program p)) 

	    val _      = print "SF top level:\n"
	    val _      = SF.pp top

	    val (rt,top) = (case top of 
				Exp e    => let val e' = elab (#env typebase) e
					    in 
						case e' of Texp (rt,e) => (rt, Exp e) 
							 | _           => raise ElabError
					    end
			      | Program p   => let val (rt',p') = elab_program (#env typebase) p
					       in 
						   (rt',Program p')  
					       end)


	    val v        = (case top of 
				Exp tree    => eval valbase tree
			      | Program p   => evalProgram valbase p)
	in
	    (v,rt)
	end
	handle  ElabError     => (print "Semantical error\n"; raise Exit OS.Process.failure )
	      | ValueError    => (print "Runtime error\n"; raise Exit OS.Process.failure )

    fun evalFromFile n =
	let
	    val f = openIn n
	    val s = inputAll f
	    val _ = closeIn f
	in
	    evalFromString  s
	end

    fun interactiveEval ()  = interactiveEval' (true, [])
    and interactiveEval' (b, l) =
	case (if b then print "> " else (); input1 TextIO.stdIn)
	 of NONE      => ()
	  | SOME #";" => (evalFromString (String.implode (rev l)); interactiveEval ())
	  | SOME s    => interactiveEval' (false, s::l)

    fun run (progname, args) =
	(let

	    val optStatus = ref NONE
	    val (opts, infiles) = (Options.getopt optStatus) args
				  
	    val _ = (case !optStatus of 
			 SOME msg => exitError (progname, msg)
		       | NONE => ())
		    
	    val {help,version} =  Options.getstate (opts)
						     
	    val _ = (if help = true then
			 exitHelp progname
		     else ())
		    
	    val _ = (if version = true then
			 exitVersion()
		     else ())
		    
	in
	    (case infiles of  []    => interactiveEval ()
			   | files => List.app (fn f => (print (f ^ " => "); 
							 printVal (evalFromFile f))) files);
	    OS.Process.exit OS.Process.success
	end)
	handle Exit status => OS.Process.exit status  
 	     | exn =>  
 	       (let val _ = TextIO.output (TextIO.stdErr,concat [progname, 
 								 ": Unexpected exception: ", 
 								 exnMessage exn, "\n"]) 
 		in OS.Process.exit OS.Process.failure  
 		end )
end

val _ = let val name = CommandLine.name()
	    val args = CommandLine.arguments()
	    val env = Posix.ProcEnv.environ()
	in
	    Main.run (name, args)
	end
