
(* 
 * Main.sml
 *
 * Code generation driver program.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

structure Main (*: MAIN*) =
struct
    open TextIO
    open SF
    open CPS
    open Prim
    open Clos
    open Alloc 

    val VERSION = "0.1"

    exception TransformError
    exception Exit of OS.Process.status 

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

    fun transformFromString s =
	let
	    fun output strm x = TextIO.outputSubstr (strm,Substring.full x)

	    val _      = print "parsing s-exp\n"

	    val sexp   = Sexp.parse (Sexp.lex (String.explode s))

	    val _      = print "parsing SXML\n"

	    val top    = fromSXML sexp

	    val _      = print "de-sugaring\n"
			 
	    val (n,p)  = case top of 
			     Program' p  => (#1 p, transform_program p)
			   | _           => raise TransformError

						  
	    val _      = print "SF type elaboration\n"


	    val strm   = TextIO.openOut ("sf.log")
	    val _      = SF.ppWithStream strm (Program p)
	    val _      = TextIO.closeOut strm
			   
	    val (rt,p) = let val (rt,p) = elab_program (#env typebase) p
			 in (rt, p) end

	    val _      = print "CPS conversion\n"

	    val _       = Symbol.reset()
	    val cps_p   = cps_transform_program p
				       
	    val strm   = TextIO.openOut ("cps.log")
	    val _      = CPS.cps_ppWithStream strm (#3 cps_p)
	    val _      = TextIO.closeOut strm
		       
	    val _      = print "CPS type check\n"

	    val _      = cps_Pt (#env cps_typebase) cps_p

	    val _      = print "prim conversion\n"

	    val _      = Symbol.reset()
	    val prim_p = prim_transform_program cps_p

	    val strm   = TextIO.openOut ("prim.log")
	    val _      = Prim.prim_ppWithStream strm (#3 prim_p)
	    val _      = TextIO.closeOut strm
				       
	    val _      = print "prim type check\n"
	    val _      = prim_Pt (#env prim_typebase) prim_p

	    val _       = print "closure program conversion\n"
	    val _       = Symbol.reset()
	    val clos_p  = clos_transform_program prim_p

	    val strm   = TextIO.openOut ("clos.log")
	    val _      = Env.app (fn b => Clos.clos_ppblockWithStream strm b) (#3 clos_p)
	    val _      = Clos.clos_ppWithStream strm (#4 clos_p)
	    val _      = TextIO.closeOut strm 
			 
	    val _      = print ("clos heap size: " ^ (Int.toString (Env.numItems (#3 clos_p))) ^ "\n")

	    val _       = print "closure type check\n"
	    val _       = clos_Pt (clos_typebase) clos_p

	    val _       = print "allocation\n"
	    val _       = Symbol.reset()
	    val alloc_p = alloc_transform_program clos_p

	    val strm   = TextIO.openOut ("alloc.log")
	    val _      = Env.app (fn((l,_),_,SOME b) => 
				    Alloc.alloc_ppblockWithStream strm (l,b)
				  | _ => ()) (#4 alloc_p)
	    val _      = Alloc.alloc_ppWithStream strm (#6 alloc_p)
	    val _      = TextIO.closeOut strm 

	    val _      = print ("alloc heap size: " ^ (Int.toString (Env.numItems (#4 alloc_p))) ^ "\n")

	    val _      = print "allocation type check\n"
	    val _      = alloc_Pt (#env alloc_typebase) alloc_p

	    val strm   = TextIO.openOut ((Symbol.name n) ^ ".ll")  
	in
	    Symbol.reset();
	    LLVMbackend.generate strm alloc_p; 
	    TextIO.closeOut strm
	end

    fun transformFromFile n =
	let
	    val f = openIn n
	    val s = inputAll f
	    val _ = closeIn f
	in
	    transformFromString  s
	end

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
	     case infiles of  []    => ()
			    | files => List.app (fn f => (print (f ^ " => "); 
							  transformFromFile f)) files;
	    OS.Process.exit OS.Process.success
	end)
	handle Exit status => OS.Process.exit status  
 	     | exn =>  
 	       (let val _ = TextIO.output (TextIO.stdErr,String.concat [progname, 
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
