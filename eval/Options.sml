
(* 
 * Options.sml
 *
 * Command-line options for the System F interpreter.
 *
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

structure Options = 
struct

structure G = GetOpt

exception Error
	      
datatype flag =  Help
	       | Version 

fun showflag (Help)         = "Help"
  | showflag (Version)      = "Version"
		   

val options = 
    [
     {short="h",
      long=["help"],
      desc=G.NoArg (fn() => Help),
      help="show help"},
     
     {short="",
      long=["version","release"],
      desc=G.NoArg (fn() => Version),
      help="show version information"}
    ]

fun optError (status) (msg) = (status := SOME msg)

fun getopt status = (G.getOpt {argOrder=G.Permute, errFn=optError status,
			       options=options}) 

fun header (progname) = concat ["Usage: ", progname, " [OPTION...] files..."]

fun usage (progname) = G.usageInfo {header=(header progname), options=options}

fun getstate (opts): {help: bool, version: bool} =

    let
	val O_HELP        = ref false
	val O_VERSION     = ref false

	fun getstate' (opt) = 
	    (case opt of 
		 Help          => O_HELP := true	
	       | Version       => O_VERSION := true)

	val _ = app getstate' opts

    in {help=(!O_HELP), version=(!O_VERSION)}
    end

end
