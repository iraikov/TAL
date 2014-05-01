
(* 
 * LLVMbackend.sml
 *
 * Code generation for LLVM.
 *
 * Copyright 2006 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

structure LLVMbackend = 
struct

open Pretty
open Alloc

exception GenerateBlockError
exception GeneratePrimError
exception GenerateInsnError
exception GenerateRegError
exception GenerateVarError
exception GenerateTypeError
exception GenerateValError
exception SemantError

fun nl () = breakWith "\n"

fun line doc       = cons (group doc,nl())
fun stmt doc       = line (group doc)

val wd = 80

val $  = (fn(s) => cons (group(text s),break()))
val &  = connect

fun name s = text ("\"" ^ ("%" ^ (Symbol.name s)) ^ "\"")

fun insn (i,x,y)   = group (nest (2, (connect (i, group (connectWith (",", x, y))))))
fun insn1 (i,x,y)  = group (nest (2, (connect (i, group (connect (x, y))))))

fun funcall (rty,f,tys,xs)  = group (& (text "call", 
					group (&(rty, cons (f, block {l=paren L,r=paren R} 2 
								     (list 2 (fn ((t,x)) => &(t,x),comma) 
									   (ListPair.zip (tys,xs))))))))
fun load (ty,ptr) = 
    insn1(text "load", &(ty,text "*"), ptr)

fun store (ty,v,ptr) = 
    insn(text "store", &(ty,v), &(&(ty,text "*"), ptr))

fun getelementptr (ty,var,idx) =
    insn1 (text "getelementptr", &(ty,text "*"), connectWith (",", var, idx))

fun cast (ty1,v,ty2) = 
    insn1 (text "cast", &(ty1, v), &(text "to", ty2))

val preamble = 
    concat [$"; primitive types and their respective indexes",
	    $"%TINT  = type int",
	    $"%TREAL = type double",
	    $"%TBOOL = type bool",
	    $"%TPROC = type uint",
	    $"%THEAP = type uint",
	    nl(),
	    $"; type used for passing values to functions",
	    $";",
	    group(cons($"%ARGTYPE = type ",
		       (block {l=brace L,r=brace R} 2 
			      (list 2 (fn(x)=>x,comma)
				    [$"%TINT", $"%TREAL",  $"%TBOOL", $"%TPROC", $"%THEAP"])))),
	    nl(),
	    nl(),
	    $"; size of the register block",
	    $"%ARGSIZE = internal constant uint 32",
	    $"; register block used for passing arguments between functions",
	    $"%ARGBLOCK = internal global [ 32 x %ARGTYPE ] zeroinitializer",
	    nl(),
	    $"; size of the I/O block",
	    $"%IOSIZE = internal constant uint 32",
	    $"; register block used for I/O",
	    $"%IOBLOCK = internal global [ 32 x %ARGTYPE ] zeroinitializer",
	    nl(),
	    $"; result register",
	    $"%RESULT = internal global %ARGTYPE zeroinitializer",
	    nl(),
	    $"; a few standard library functions",
	    $"declare void %llvm.memcpy.i32(sbyte*, sbyte*, uint, uint)",
	    $"declare double %llvm.sqrt.f64(double)",
	    $"declare double %clib.powi(double, int)",
	    $"declare int    %clib.round(double)", 	    
	    $"declare double %clib.exp(double)", 	    
	    $"declare double %clib.sin(double)", 
	    $"declare double %clib.cos(double)", 
	    $"declare double %clib.tan(double)", 
	    $"declare double %clib.asin(double)",
	    $"declare double %clib.acos(double)",
	    $"declare double %clib.atan(double)",
	    $"declare double %clib.ln(double)",  
	    $"declare double %clib.sinh(double)",
	    $"declare double %clib.cosh(double)",
	    $"declare double %clib.tanh(double)",
	    $("declare void %iota ({ uint, %TREAL* } *, " ^
	      "{ %TPROC, %THEAP } *,  %TREAL, %TREAL, %TREAL)"),
	    nl(),
	    $"; %toplevel is the state machine function",
	    $"declare void %toplevel (uint)",
	    nl()]

val iint  = text "uint 0"
val ireal = text "uint 1"
val ibool = text "uint 2"
val iproc = text "uint 3"
val iheap = text "uint 4"

fun isHeapType t =
    (case t of
	 Nil   => false
       | Unit  => false
       | Bool  => false
       | Int   => false
       | Real  => false
       | Arrow _  => false
       | _ => true)

fun isAtomType A k = 
    List.exists (fn(x)=>x=k) (#atomTyp A)

fun isUnitType A k = (k = (#unitTyp A))

fun generate_typename (k) = text ("%T" ^ (Int.toString (ttInt k)))

val code_fld  = Symbol.symbol "%code"
val env_fld   = Symbol.symbol "%env"

fun generate_typedef (t,k) =
    let
	fun typedef (t,k) = stmt (binop 2 (generate_typename k, text "=",
					   group (&(text "type", group t))))
				    
    in
	case t of 
	    Unit     => empty()
	  | Nil      => typedef (text "%THEAP",k)
	  | Bool     => typedef (text "%TBOOL",k)
	  | Int      => typedef (text "%TINT",k)
	  | Real     => typedef (text "%TREAL",k)
	  | (Pair ((t,k'),_)) =>
	    (let
		 val fs  = list 2 (fn(x)=>x,comma)
				[generate_typename k', &(generate_typename k', text "*")]
	     in 
		 typedef (block {l=brace L,r=brace R} 8 fs,k)
	     end)
	    
	  | (Vector ((t,k'),_))  =>
	    (let 
		 val fs  = list 2 (fn(x)=>x,comma)
				[text "uint", &(generate_typename k', text "*")]
	     in 
		 typedef (block {l=brace L,r=brace R} 8 fs,k)
	     end)
	    
	  | (Record (fs,_)) =>
	    (let val fs  =  list 2 (fn(s,(Unit,k),i)=>text "%THEAP"
				    |(s,(t,k),i)=> generate_typename k,comma) fs
	     in 
		 typedef (block {l=brace L,r=brace R} 8 fs,k)
	     end)
		 
	  | (Arrow _) => typedef (text "%TPROC",k)
			 
	  | (Tvar x)  => typedef (text "%THEAP",k)
	  | (Exists (a,(t,_))) => generate_typedef (t,k)
    end
				  

fun generate_heap h =
    Env.foldl (fn(((l,k),_,NONE),ax) => 
	       let val x = name l
	       in 
		   (stmt (binop 2 (x, text "=", &(text "internal global", 
						  &(generate_typename k, text "zeroinitializer"))))) :: ax
	       end
	     | ((l,_,_),ax) => ax) [] h


fun generate_arg (x,t,k) =
    let 
	val r  = case t of
		     Int     => iint
		   | Real    => ireal 
		   | Bool    => ibool
		   | Arrow _ => iproc
		   | _       => iheap
    in 
	getelementptr (text "[ 32 x %ARGTYPE ]", x, r)
    end


fun generate_res (x,t,k) =
    let 
	val r  = case t of
		     Int     => iint
		   | Real    => ireal
		   | Bool    => ibool
		   | Arrow _ => iproc
		   | _       => iheap
    in 
	getelementptr (text "%ARGTYPE", x, r)
    end


fun generate_prim ("add",  [(Int,_)],  [x1,x2]) = ([], SOME(insn (text "add %TINT",    x1, x2)))
  | generate_prim ("add",  [(Real,_)], [x1,x2]) = ([], SOME(insn (text "add %TREAL",   x1, x2)))
  | generate_prim ("sub",  [(Int,_)],  [x1,x2]) = ([], SOME(insn (text "sub %TINT",    x1, x2)))
  | generate_prim ("sub",  [(Real,_)], [x1,x2]) = ([], SOME(insn (text "sub %TREAL",   x1, x2)))
  | generate_prim ("mul",  [(Int,_)],  [x1,x2]) = ([], SOME(insn (text "mul %TINT ",   x1, x2)))
  | generate_prim ("mul",  [(Real,_)], [x1,x2]) = ([], SOME(insn (text "mul %TREAL ",  x1, x2)))
  | generate_prim ("div",  [(Int,_)],  [x1,x2]) = ([], SOME(insn (text "div %TINT ",  x1, x2)))
  | generate_prim ("div",  [(Real,_)], [x1,x2]) = ([], SOME(insn (text "div %TREAL ", x1, x2)))
  | generate_prim ("pow",  [], [x1,x2])     = ([], SOME(funcall (text "double",
								 text "%llvm.powi.f64", 
								 [text "%TINT", text "%TREAL"], [x2,x1])))

  | generate_prim ("eq",   [(Int,_)],  [x1,x2])  = ([], SOME(insn (text "seteq int", x1, x2)))
  | generate_prim ("eq",   [(Real,_)], [x1,x2])  = ([], SOME(insn (text "seteq double", x1, x2)))
  | generate_prim ("eq",   [(Bool,_)], [x1,x2])  = ([], SOME(insn (text "seteq bool", x1, x2)))
  | generate_prim ("less", [(Int,_)],  [x1,x2])  = ([], SOME(insn (text "setlt int", x1, x2)))
  | generate_prim ("less", [(Real,_)], [x1,x2])  = ([], SOME(insn (text "setlt double", x1, x2)))

  | generate_prim ("or",   [], [x1,x2]) = ([], SOME(insn (text "or  %TBOOL", x1, x2)))
  | generate_prim ("and",  [], [x1,x2]) = ([], SOME(insn (text "and %TBOOL", x1, x2)))
  | generate_prim ("not",  [], [x1])    = ([], SOME(insn (text "seteq bool", text "false", x1)))
	           
  | generate_prim ("sqrt",  [], [x1]) = ([], SOME(funcall (text "double",
						      text "%llvm.sqrt.f64", [text "%TREAL"], [x1])))
  | generate_prim ("round", [], [x1]) = ([], SOME(funcall (text "int",
						      text "%clib.round",  [text "%TREAL"], [x1])))
  | generate_prim ("exp",   [], [x1]) = ([], SOME(funcall (text "double",
						      text "%clib.exp",  [text "%TREAL"], [x1])))
  | generate_prim ("sin",   [], [x1]) = ([], SOME(funcall (text "double",
						      text "%clib.sin",  [text "%TREAL"], [x1])))  
  | generate_prim ("cos",   [], [x1]) = ([], SOME(funcall (text "double",
						      text "%clib.cos",  [text "%TREAL"], [x1]))) 
  | generate_prim ("tan",   [], [x1]) = ([], SOME(funcall (text "double",
						      text "%clib.tan",  [text "%TREAL"], [x1])))
  | generate_prim ("asin",  [], [x1]) = ([], SOME(funcall (text "double",
						      text "%clib.asin", [text "%TREAL"], [x1])))
  | generate_prim ("acos",  [], [x1]) = ([], SOME(funcall (text "double",
						      text "%clib.acos", [text "%TREAL"], [x1])))
  | generate_prim ("atan",  [], [x1]) = ([], SOME(funcall (text "double",
						      text "%clib.atan", [text "%TREAL"], [x1])))
  | generate_prim ("ln",    [], [x1]) = ([], SOME(funcall (text "double",
						      text "%clib.ln",   [text "%TREAL"], [x1])))
  | generate_prim ("sinh",  [], [x1]) = ([], SOME(funcall (text "double",
						      text "%clib.sinh", [text "%TREAL"], [x1])))
  | generate_prim ("cosh",  [], [x1]) = ([], SOME(funcall (text "double",
						      text "%clib.cosh", [text "%TREAL"], [x1])))
  | generate_prim ("tanh",  [], [x1]) = ([], SOME(funcall (text "double",
						      text "%clib.tanh", [text "%TREAL"], [x1])))

  | generate_prim ("car",  [(Pair ((t,k),p),_)], [x1]) = 
    let
	val ty = generate_typename k
	val ptr = Symbol.freshPrefix "%ptr1"
    in
	([binop 2 (text (Symbol.name ptr), text "=", 
		   getelementptr (ty, x1, text "uint 0"))],
	 SOME(load(generate_typename k,text (Symbol.name ptr))))
    end

  | generate_prim ("cdr",  [(t,k)], [x1]) = 
    let
	val ty = generate_typename k
	val ptr = Symbol.freshPrefix "%ptr2"
    in
	([binop 2 (text (Symbol.name ptr), text "=", 
		   getelementptr (ty, x1, text "uint 1"))],
	 SOME(load(&(generate_typename k,text "*"),text (Symbol.name ptr))))
    end

  | generate_prim ("iota",  [], xs as [l,x0,x1,x2,x3]) = 
	([funcall (text "void",
		   text "%iota", 
		   [text "{ uint, %TREAL* } *", 
		    text "{ %TPROC, %THEAP } *", 
		    text "%TREAL", text "%TREAL", text "%TREAL"], xs)],
	 NONE)

  | generate_prim ("cons", [(t,k)], [l,x1,x2]) = 
    let
	val ty   = generate_typename k
	val car  = Symbol.freshPrefix "%ptr3"
	val cdr  = Symbol.freshPrefix "%ptr4"
    in 
	([stmt(binop 2 (text (Symbol.name car), text "=", 
			getelementptr (ty, l, text "uint 0, uint 0"))),
	  stmt(binop 2 (text (Symbol.name cdr), text "=", 
			getelementptr (ty, l, text "uint 0, uint 1"))),
	  stmt(store (ty, x1, text (Symbol.name car))), 
	  stmt(store (&(ty,text "*"), x2, text (Symbol.name cdr)))], 
	 SOME l)
    end

  | generate_prim ("read",  [], [x1])    = 
    let
	val port  = connectWith (",", connectWith (",", text "%IOBLOCK", text "uint 0"),
				 &(text "int",x1))
	val ptr   = Symbol.freshPrefix "%ptr5"    
    in
	([stmt (binop 2 (text (Symbol.name ptr), text "=", 
			 getelementptr (text "[ 32 x %ARGTYPE ]", port, ireal)))],
	 SOME(stmt (load (text "%TREAL",text (Symbol.name ptr)))))
    end

  | generate_prim ("write", [], [x1,x2]) = 
    let
	val port  = connectWith (",", connectWith (",", text "%IOBLOCK", text "uint 0"),
				 &(text "int",x1))
	val ptr   = Symbol.freshPrefix "%ptr6"    
    in
	([stmt (binop 2 (text (Symbol.name ptr), text "=", 
			 getelementptr (text "[ 32 x %ARGTYPE ]", port, ireal))),
	 stmt (store (text "%TREAL",x2, text (Symbol.name ptr)))],
	 NONE)
    end
  | generate_prim (s, _, _) = raise GeneratePrimError before print ("s = " ^ s ^ "\n")


fun generate_value H A (NilVal)         = ([], text "null")
  | generate_value H A (UnitVal)        = ([], text "undef")
  | generate_value H A (Var x)          = ([], name x)
  | generate_value H A (BoolVal true)   = ([], text "true") 
  | generate_value H A (BoolVal false)  = ([], text "false") 
  | generate_value H A (IntNum i)       = let val s = Int.toString (Int.abs i)
					  val s = if i<0 then "-"^s else s
				      in ([], text s) end
  | generate_value H A (RealNum r)      = let val s = Real.fmt (StringCvt.EXACT) (Real.abs r)
					    val s = if r<0.0 then "-"^s else s
				      in ([], text s) end
  | generate_value H A (Pack ((t,kt),(v,kv),(t',kt')))  = 
    let val (d,v') = generate_value H A v
	val xcast  = Symbol.freshPrefix "%cast"
	val vty    = &(generate_typename kv,text "*")
	val ty'    = &(generate_typename kt',text "*")
    in
	(d @ [stmt(binop 2 (text (Symbol.name xcast),text "=",cast(vty,v',ty')))],
	 text (Symbol.name xcast))
    end
  | generate_value H A (Label l)        = 
    let
	val ((_,k),t,_) = Env.look (l,H)
	val ty    = generate_typename k
	val ptr   = Symbol.freshPrefix "%ptr7"
    in
	(* ([stmt(binop 2 (text (Symbol.name ptr), text "=", 
			getelementptr (ty, name l, text "uint 0")))],
	 text (Symbol.name ptr)) *)
	case t of Arrow _ => ([], text (Int.toString (Symbol.toInt l)))
		| _ => ([], name l)
    end
  | generate_value H A (Ref (mode,(x,kx),(v,kv)))  =
    let	
	val vty    = generate_typename kv
	val ty     = generate_typename kx

	val (d,x') = generate_value H A x
	val ptr    = Symbol.freshPrefix "%ptr8"

	val d'  = case v of
		      Field (f,i)  => [stmt (binop 2 (text (Symbol.name ptr), text "=", 
						      getelementptr (ty, connectWith(",",x',text "uint 0"), 
								     text ("uint " ^ (Int.toString i)))))]
		    | IntNum i     => (let val pval = Symbol.freshPrefix "%val"
					   val pptr = Symbol.freshPrefix "%ptr81"
				       in
					   [stmt (binop 2 (text (Symbol.name pptr), text "=", 
							   getelementptr (ty, connectWith(",",x',text "uint 0"), 
									  text ("uint 1")))),
					    stmt (binop 2 (text (Symbol.name pval), text "=", 
							   load (&(vty,text "*"), text (Symbol.name pptr)))),
					    stmt (binop 2 (text (Symbol.name ptr), text "=", 
							   getelementptr (vty, text (Symbol.name pval),
									  text ("int " ^ (Int.toString i)))))]
				       end)
		    | Var i        => (let val pval = Symbol.freshPrefix "%val"
					   val pptr = Symbol.freshPrefix "%ptr82"
						      
				       in
					   [stmt (binop 2 (text (Symbol.name pptr), text "=", 
							   getelementptr (ty, connectWith(",",x',text "uint 0"), 
									  text ("uint 1")))),
					    stmt (binop 2 (text (Symbol.name pval), text "=", 
							   load (&(vty,text "*"), text (Symbol.name pptr)))),
					    stmt (binop 2 (text (Symbol.name ptr), text "=", 
							   getelementptr (vty, text (Symbol.name pval),
									  &(text "int",name i))))]
				       end)
		    | _ => raise GenerateValError
		      
    in
	if (isAtomType A kv) andalso mode=READ
	then (let val refval = Symbol.freshPrefix "%val"
	      in 
		  (d @ d' @ [stmt (binop 2 (text (Symbol.name refval), text "=", 
					    load (vty, text (Symbol.name ptr))))],
		   text (Symbol.name refval))
	      end)
	 else (d @ d', text (Symbol.name ptr))
    end

  | generate_value H A (Prim (Var p,t,vs))  = 
    let 
	val (dargs,args) = foldr (fn(v,(ds,vs)) => 
				    let val (d',v') = generate_value H A v
				    in
					(d'@ds, v'::vs)
				    end) ([],[]) vs

	val (dprim,v) = generate_prim (Symbol.name p,t,args) 
    in 
	case v of 
	    SOME v => (let val x = Symbol.freshPrefix "%prim"
		       in
			   (dargs @ dprim @ [stmt(binop 2 (text (Symbol.name x), text "=", v))],  
			    text (Symbol.name x))
		       end)
	  | NONE  => (dargs @ dprim, text "undef")
    end

  | generate_value H A v = (print "generate_value v: v = " ; 
			    alloc_ppval v;
			    print "\n";
			    raise GenerateValError)


fun generate_decl1 H A (d, ax) =
    let
    	fun vhref (t,v,h) =  if t then h() else v()

	fun assign (s,v,k) = 
	    (let
		 val (dv,v')  = generate_value H A v
		 val ty       = generate_typename k
		 val ptr      = Symbol.freshPrefix "%ptr9"
	     in 
		 dv @ (if isAtomType A k 
		       then [stmt(binop 2 (text (Symbol.name ptr), text "=", &(text "alloca", ty))),
			     stmt(store (ty, v', text (Symbol.name ptr))),
			     stmt(binop 2 (name s, text "=", load(ty,text (Symbol.name ptr))))]
		       else [stmt(binop 2 (name s, text "=", getelementptr (ty, v', text "uint 0")))])
	     end)
    in
	case d of
	    Assign ((s,_),(v,k)) =>  if isUnitType A k then ax else (assign(s,v,k)) @ ax
	    
	  | Unpack (a,(x,kx),(v,kv)) =>  if isUnitType A kv then ax else (assign(x,v,kv)) @  ax
	    
	  | Memcpy ((l,kl),(x,_),(d,_),(s,k)) => 
	    if isUnitType A k 
	    then (let
		      val lty      = generate_typename kl
		  in 
		      [stmt(binop 2 (name x, text "=", getelementptr (lty, name l, text "uint 0")))] @ ax
		  end)
	    else (let 
		      val ty       = generate_typename k
		      val lty      = generate_typename kl
		      val (dd,d')  = generate_value H A d
		      val (ds,s')  = generate_value H A s
		  in 
		      dd @ ds @
		      (if (isAtomType A k) 
		       then [stmt(store (ty,s',d')),
			     stmt(binop 2 (name x, text "=", getelementptr (lty, name l, text "uint 0")))]
		       else (let
				  val dcast    = Symbol.freshPrefix "%cast"
				  val scast    = Symbol.freshPrefix "%cast"
				  val sizeptr  = Symbol.freshPrefix "%sizeptr"
				  val sizeof   = Symbol.freshPrefix "%sizeof"
			      in
				  [stmt(binop 2 (text (Symbol.name sizeptr), text "=", 
						 getelementptr (ty, text "null", text "uint 1"))),
				   stmt(binop 2 (text (Symbol.name sizeof), text "=", 
						 cast(&(ty,text "*"),text (Symbol.name sizeptr),text "uint"))),
				   stmt(binop 2 (text (Symbol.name dcast), text "=", 
						 cast(&(ty,text "*"),d',text "sbyte *"))),
				   stmt(binop 2 (text (Symbol.name scast), text "=", 
						 cast(&(ty,text "*"),s',text "sbyte *"))),
				   stmt(funcall (text "void",
						 text "%llvm.memcpy.i32", 
						 [text "sbyte *", text "sbyte *", text "uint", text "uint"],
						 [text (Symbol.name dcast),
						  text (Symbol.name scast),
						  text (Symbol.name sizeof), 
						  text "0"])),
				   stmt(binop 2 (name x, text "=", getelementptr (lty, name l, text "uint 0")))]
			      end)) @ ax
		  end)
	    
	  | Lmalloc (l,t)   => ax
	  | Rmalloc (l,sts) => ax
			       
	  | Vmalloc (l,(t,k),0) =>
	    let
		val fs  = list 2 (fn(x)=>x,comma)
			       [text "uint", &(generate_typename k, text "*")]
		val ty  = block {l=brace L,r=brace R} 8 fs
		val vty = generate_typename k
 
		val sizeptr = Symbol.freshPrefix "%ptr11"
		val dataptr = Symbol.freshPrefix "%ptr12"
	    in
		[stmt(binop 2 (text (Symbol.name sizeptr), text "=", 
				    getelementptr (ty, name l, text "uint 0, uint 0"))),
		 stmt(binop 2 (text (Symbol.name dataptr), text "=", 
			       getelementptr (ty, name l, text "uint 0, uint 1"))),
		 stmt(store (text "uint",text "0",text (Symbol.name sizeptr))),
		 stmt(store (&(vty,text "*"),text "null",text (Symbol.name dataptr)))] @ ax
	    end
	    
	  | Vmalloc (l,(t,k),n) =>
	    let
		val fs  = list 2 (fn(x)=>x,comma)
			       [text "uint", &(generate_typename k, text "*")]
		val ty  = block {l=brace L,r=brace R} 8 fs
		val vty = generate_typename k

		val sizeptr = Symbol.freshPrefix "%ptr13"
		val dataptr = Symbol.freshPrefix "%ptr14"
		val mptr = Symbol.freshPrefix "%ptr15"
	    in
		[stmt(binop 2 (text (Symbol.name sizeptr), text "=", 
			       getelementptr (ty, name l, text "uint 0, uint 0"))),
		 stmt(binop 2 (text (Symbol.name dataptr), text "=", 
			       getelementptr (ty, name l, text "uint 0, uint 1"))),
		 stmt(binop 2 (text (Symbol.name mptr), text "=", 
			       insn(text "malloc", ty, text ("uint " ^ (Int.toString n))))),
		 stmt(store (text "uint",text "0",text (Symbol.name sizeptr))),
		 stmt(store (&(vty,text "*"),text (Symbol.name mptr),text (Symbol.name dataptr)))] @ ax
	    end

    end
    
fun generate_decl H A [] = empty()
  | generate_decl H A ds = (list 2 (fn x => x,break) (foldr (generate_decl1 H A) [] ds))

fun generate_insn (U: tmap) (H: heap) A x  =
    case x of 
	(App ((fv,fk),vs)) =>
	(let
	     val regs      = List.tabulate (length vs, fn(i) => i)
	     val (fd,fv')  = generate_value H A fv
			 
	     val ds  = 
		 ListPair.foldr
		     (fn ((v,k),i,ax) => 
			 let 
			     val t  = case ttFind(k,U) of 
					  SOME t => t
					| NONE   => 
					  (print ("generate_insn: App: k = " ^ (Int.toString (ttInt k)) ^ "\n");
					   print "\n";
					   print "generate_insn: App: v = ";
					   alloc_ppval v;
					   print "\n";
					   print "generate_insn: App: fv = ";
					   alloc_ppval fv;
					   print "\n";
					   raise GenerateInsnError)
			 in 
			     case t of 
				 Unit => ax
			       | (Record ([],_)) => ax
			       | _ => 
				 let
				     val ty    = generate_typename k
				     val reg   = connectWith (",", connectWith (",", text "%ARGBLOCK",  text "uint 0"),
							      text ("uint " ^ (Int.toString i)))
				     val ptr   = Symbol.freshPrefix "%ptr16"

				     val (d,v')  = generate_value H A v 
				 in
				     d @ [stmt (binop 2 (text (Symbol.name ptr),  
							 text "=", generate_arg (reg,t,k)))] @
					  (if (isAtomType A k) 
					   then [stmt (store (ty,v',text (Symbol.name ptr)))]
					   else (let
						     val dst      = text (Symbol.name ptr)
						     val src      = v'
						     val dcast    = Symbol.freshPrefix "%cast"
						     val scast    = Symbol.freshPrefix "%cast"
						     val sizeptr  = Symbol.freshPrefix "%sizeptr"
						     val sizeof   = Symbol.freshPrefix "%sizeof"
						 in
						     [stmt(binop 2 (text (Symbol.name sizeptr), text "=", 
								    getelementptr (ty, text "null", text "uint 1"))),
						      stmt(binop 2 (text (Symbol.name sizeof), text "=", 
								    cast(&(ty,text "*"),text (Symbol.name sizeptr),
									 text "uint"))),
						      stmt(binop 2 (text (Symbol.name dcast), text "=", 
								    cast(&(text "%THEAP",text "*"),dst,text "sbyte *"))),
						      stmt(binop 2 (text (Symbol.name scast), text "=", 
								    cast(&(ty,text "*"),src,text "sbyte *"))),
						      stmt(funcall (text "void",
								    text "%llvm.memcpy.i32", 
								    [text "sbyte *", text "sbyte *", 
								     text "uint", text "uint"],
								    [text (Symbol.name dcast),
								     text (Symbol.name scast),
								     text (Symbol.name sizeof), 
								     text "0"]))]
						 end))
				     @ ax
				 end
			 end) fd (vs,regs)
		 
	     val ty   = generate_typename fk
	 in 
	     list 2 (fn x => x,break) 
		  (ds @ ([stmt(store (ty, fv', text "%toplevel_mux")),
			  stmt(text "br label \"%toplevel_begin\"")]))
	 end)
	
      | If0 ((v,_),e1,e2) =>
	let
	    val iftrue   = Symbol.freshPrefix "iftrue"
	    val iffalse  = Symbol.freshPrefix "iffalse"
	    val (d,v')   = generate_value H A v
	in
	    concat (d @ [stmt (ifthen {i=text "br",t=comma(),e=comma()} 2 
				      (group(&(text "bool",v')),
				       group(&(text "label",name iftrue)), 
				       group(&(text "label",name iffalse)))),
			 stmt (cons((name iftrue),text ":")),
			 generate_insn U H A e1,
			 stmt (cons((name iffalse),text ":")),
			 generate_insn U H A e2])
	end
	
      | Halt (t,(v,k)) =>
	let
	    val ty      = generate_typename k
	    val (d,v')  = generate_value H A v
	    val r       = generate_res (connectWith(",",text "%RESULT",text "uint 0"),t,k)
	    val ptr     = Symbol.freshPrefix "%ptr17"
	in 
	    concat (d @ [stmt (binop 2 (text (Symbol.name ptr), text "=", r))] @
			 (if isAtomType A k 
			  then [stmt (store (ty,v',text (Symbol.name ptr)))]
			  else (let
				    val dst      = text (Symbol.name ptr)
				    val src      = v'
				    val dcast    = Symbol.freshPrefix "%cast"
				    val scast    = Symbol.freshPrefix "%cast"
				    val sizeptr  = Symbol.freshPrefix "%sizeptr"
				    val sizeof   = Symbol.freshPrefix "%sizeof"
				in
				    [stmt(binop 2 (text (Symbol.name sizeptr), text "=", 
						   getelementptr (ty, text "null", text "uint 1"))),
				     stmt(binop 2 (text (Symbol.name sizeof), text "=", 
						   cast(&(ty,text "*"),text (Symbol.name sizeptr),
							text "uint"))),
				     stmt(binop 2 (text (Symbol.name dcast), text "=", 
						   cast(&(text "%THEAP",text "*"),dst,text "sbyte *"))),
				     stmt(binop 2 (text (Symbol.name scast), text "=", 
						   cast(&(ty,text "*"),src,text "sbyte *"))),
				     stmt(funcall (text "void",
						   text "%llvm.memcpy.i32", 
						   [text "sbyte *", text "sbyte *", 
						    text "uint", text "uint"],
						   [text (Symbol.name dcast),
						    text (Symbol.name scast),
						    text (Symbol.name sizeof), 
						    text "0"]))]
				end)) @
			 [$"br label \"%l0\""])
	end
	
      | Let (ds,e) => &(generate_decl H A ds, generate_insn U H A e)

    
fun generate_block (U: tmap) (H: heap) A (l,([],xs,e)) =
    let
	val regs      = List.tabulate (length xs, fn(i) => i)
	val loadregs  = 
	    foldr (fn (((s,(Unit,k)),i),ax)   => ax
		    | (((s,(t,k)),i),ax) => 
		      (let 
			   val ty = generate_typename k
			   val reg = connectWith (",", connectWith (",", text "%ARGBLOCK", text "uint 0"),
						  text ("uint " ^ (Int.toString i)))
			   val ptr = Symbol.freshPrefix "%ptr18"
		       in
			   [stmt (binop 2 (text (Symbol.name ptr),  text "=", generate_arg (reg,t,k)))] @
			   (if isAtomType A k
			    then [stmt (binop 2 (name s, text "=", load (ty,text (Symbol.name ptr))))]
			    else (let
				      val src      = text (Symbol.name ptr)
				      val dst      = name s
				      val dcast    = Symbol.freshPrefix "%cast"
				      val scast    = Symbol.freshPrefix "%cast"
				      val sizeptr  = Symbol.freshPrefix "%sizeptr"
				      val sizeof   = Symbol.freshPrefix "%sizeof"
			      in
				  [stmt(binop 2 (name s, text "=", &(text "alloca", ty))),
				   stmt(binop 2 (text (Symbol.name sizeptr), text "=", 
						 getelementptr (ty, text "null", text "uint 1"))),
				   stmt(binop 2 (text (Symbol.name sizeof), text "=", 
						 cast(&(ty,text "*"),text (Symbol.name sizeptr),text "uint"))),
				   stmt(binop 2 (text (Symbol.name dcast), text "=", 
						 cast(&(ty,text "*"),dst,text "sbyte *"))),
				   stmt(binop 2 (text (Symbol.name scast), text "=", 
						 cast(&(text "%THEAP",text "*"),src,text "sbyte *"))),
				   stmt(funcall (text "void",
						 text "%llvm.memcpy.i32", 
						 [text "sbyte *", text "sbyte *", text "uint", text "uint"],
						 [text (Symbol.name dcast),
						  text (Symbol.name scast),
						  text (Symbol.name sizeof), 
						  text "0"]))]
			      end))
			   @ ax
		       end)) [] (ListPair.zip (xs,regs))
    in
	procblk {h=line (cons(name l, text ":")), l=empty(),r=empty()} 2
		(list 2 (fn x => x,break) (loadregs @ [generate_insn U H A e]))
    end
  |  generate_block U H A (l,_) = raise GenerateBlockError
    
fun generate_jumptbl (h: heap) =
    (Env.foldl (fn(((l,_),_,SOME code),ax) => 
		  ((line (connectWith (",",&(text "uint", text (Int.toString (Symbol.toInt l))), 
				       &(text "label", name l)))) :: ax)
		| (((l,_),_,NONE),ax) => ax)
	       ([]) h)

fun generate strm (s,t,u: tmap,h: heap,p,is) =
    let
	fun outf x = printf strm (format wd x)

	val A  = ttFoldli (fn(k,Unit,{unitTyp,atomTyp}) => {unitTyp=k,atomTyp=atomTyp}
			   | (k,t,A as {unitTyp,atomTyp}) => 
			     let val A' = if isHeapType t 
					  then A else {unitTyp=unitTyp,atomTyp=k::atomTyp}
			     in A' end) {unitTyp=ttNull,atomTyp=[]} u

    in
	outf preamble;

	outf(nl());

	ttAppi (fn(k,t) => let 
		      val g = generate_typedef (t,k)
		  in
		      printf strm (format wd g)
		  end) u;

	outf(nl());

	app outf (generate_heap h);

	outf(nl());

	outf($"implementation");

	outf(nl());

	outf (procblk {h= ($ "void %toplevel (uint %faddress)"), l=brace L,r=brace R} 2
		      (list 2 (fn x => x,break)
			    ([$"%toplevel_mux = alloca uint",
			      stmt(store (text "uint", text "%faddress", text "%toplevel_mux")),
			      stmt(text "br label \"%toplevel_begin\""),
			      $"\"%toplevel_begin\":",
			      stmt(load (text "uint", text "%toplevel_mux")),
			      stmt(&(text "switch", 
				      &(text "uint %0, label \"%l0\"",
					block {l=bracket L,r=bracket R} 2
					      (list 2 (fn(x) => x,break)
						    (generate_jumptbl h))))),
			      $"\"%l0\":",
			      block {l=empty(),r=empty()} 2 ($ "ret void"),
			      $"\"%l1\":",
			      stmt(block {l=empty(),r=empty()} 2 (generate_insn u h A is))] @
			     (List.mapPartial (fn((l,k),_,SOME code) => 
						  SOME (stmt (generate_block u h A (l,code)))
					      | (_,_,NONE)          => NONE)
                                              (Env.listItems h)))));

	outf (nl())
    end

end
