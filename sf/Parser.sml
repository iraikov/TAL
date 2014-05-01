
(* 
 * Parser.sml
 *
 * System F s-expression parser.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

structure Parser : PARSER =
struct

open Elab

type pos = int
type name = {name: symbol, pos: pos}

fun symofname {pos=_, name=sym} = sym
fun posofname {pos=pos, name=_} = pos
fun makename  (n,p) = {name=n, pos=p}

fun name0 s = makename (s,0)


val  SF_ABS as (_,T_SF_ABS)     = Symbol.symbol "sf:abs"
val  SF_APP as (_,T_SF_APP)     = Symbol.symbol "sf:app"
val  SF_ARGS as (_,T_SF_ARGS)   = Symbol.symbol "sf:args"
val  SF_ARROW as (_,T_SF_ARROW) = Symbol.symbol "sf:arrow"
val  SF_BODY as (_,T_SF_BODY)   = Symbol.symbol "sf:body"
val  SF_BOOL as (_,T_SF_BOOL)   = Symbol.symbol "sf:bool"
val  SF_BOOL_CONST as (_,T_SF_BOOL_CONST)   = Symbol.symbol "sf:bool_const"
val  SF_CHANNEL as (_,T_SF_CHANNEL) = Symbol.symbol "sf:channel"
val  SF_CLAUSE as (_,T_SF_CLAUSE)   = Symbol.symbol "sf:clause"
val  SF_CLAUSES as (_,T_SF_CLAUSES) = Symbol.symbol "sf:clauses"
val  SF_COND as (_,T_SF_COND)     = Symbol.symbol "sf:cond"
val  SF_CONST as (_,T_SF_CONST)   = Symbol.symbol "sf:const"
val  SF_DEFAULT as (_,T_SF_DEFAULT) = Symbol.symbol "sf:default"
val  SF_E1 as (_,T_SF_E1)         = Symbol.symbol "sf:e1"
val  SF_E2 as (_,T_SF_E2)         = Symbol.symbol "sf:e2"
val  SF_E3 as (_,T_SF_E3)         = Symbol.symbol "sf:e3"
val  SF_EXP as (_,T_SF_EXP)       = Symbol.symbol "sf:exp"
val  SF_ENV as (_,T_SF_ENV)       = Symbol.symbol "sf:env"
val  SF_ENVEN as (_,T_SF_ENVEN)   = Symbol.symbol "sf:enven"
val  SF_FCON as (_,T_SF_FCON)     = Symbol.symbol "sf:fcon" 
val  SF_FIELD as (_,T_SF_FIELD)   = Symbol.symbol "sf:field" 
val  SF_FORMAL as (_,T_SF_FORMAL) = Symbol.symbol "sf:formal"
val  SF_FORMALS as (_,T_SF_FORMALS) = Symbol.symbol "sf:formals"
val  SF_IF as (_,T_SF_IF)         = Symbol.symbol "sf:if"
val  SF_INDEX as (_,T_SF_INDEX)   = Symbol.symbol "sf:index"
val  SF_INT as (_,T_SF_INT)       = Symbol.symbol "sf:int"
val  SF_INT_CONST as (_,T_SF_INT_CONST)       = Symbol.symbol "sf:int_const"
val  SF_LAMBDA as (_,T_SF_LAMBDA) = Symbol.symbol "sf:lambda"
val  SF_LET as (_,T_SF_LET)       = Symbol.symbol "sf:let"
val  SF_PAIR as (_,T_SF_PAIR)     = Symbol.symbol "sf:pair"
val  SF_NAME as (_,T_SF_NAME)     = Symbol.symbol "sf:name"
val  SF_NIL as (_,T_SF_NIL)       = Symbol.symbol "sf:nil"
val  SF_NIL_CONST as (_,T_SF_NIL_CONST)       = Symbol.symbol "sf:nil_const"
val  SF_PROGRAM  as (_,T_SF_PROGRAM) = Symbol.symbol "sf:program"
val  SF_RCON as (_,T_SF_RCON)        = Symbol.symbol "sf:rcon"
val  SF_REAL as (_,T_SF_REAL)        = Symbol.symbol "sf:real"
val  SF_REAL_CONST as (_,T_SF_REAL_CONST) = Symbol.symbol "sf:real_const"
val  SF_RECORD as (_,T_SF_RECORD)    = Symbol.symbol "sf:record"  
val  SF_RSEL as (_,T_SF_RSEL)        = Symbol.symbol "sf:rsel"
val  SF_VAR as (_,T_SF_VAR)          = Symbol.symbol "sf:var"
val  SF_TLAM as (_,T_SF_TLAM)   = Symbol.symbol "sf:tlam"
val  SF_TAPP as (_,T_SF_TAPP)   = Symbol.symbol "sf:tapp"
val  SF_TABS as (_,T_SF_TABS)   = Symbol.symbol "sf:tabs"
val  SF_TARGS as (_,T_SF_TARGS) = Symbol.symbol "sf:targs"
val  SF_TCON as (_,T_SF_TCON)   = Symbol.symbol "sf:tcon"
val  SF_TENV as (_,T_SF_TENV)   = Symbol.symbol "sf:tenv"
val  SF_TLET as (_,T_SF_TLET)   = Symbol.symbol "sf:tlet"
val  SF_TOPLEVEL as (_,T_SF_TOPLEVEL) = Symbol.symbol "sf:toplevel"
val  SF_TVAL  as (_,T_SF_TVAL)  = Symbol.symbol "sf:tval"
val  SF_TVAR as (_,T_SF_TVAR)   = Symbol.symbol "sf:tvar"
val  SF_TVARS as (_,T_SF_TVARS) = Symbol.symbol "sf:tvars"
val  SF_TFORMALS as (_,T_SF_TVARS) = Symbol.symbol "sf:tformals"
val  SF_TYPE as (_,T_SF_TYPE)   = Symbol.symbol "sf:type"
val  SF_UNIT as (_,T_SF_UNIT)   = Symbol.symbol "sf:unit"
val  SF_UNIT_CONST as (_,T_SF_UNIT_CONST)    = Symbol.symbol "sf:unit_const"
val  SF_VAL as (_,T_SF_VAL)   = Symbol.symbol "sf:val"
val  SF_VARS as (_,T_SF_VARS) = Symbol.symbol "sf:vars"
val  SF_VECTOR as (_,T_SF_VECTOR) = Symbol.symbol "sf:vector"
val  SF_VCON as (_,T_SF_VCON)     = Symbol.symbol "sf:vcon"
val  SF_VREF as (_,T_SF_VREF)     = Symbol.symbol "sf:vref"

exception ParserError

fun unparse ({uname:  name -> Sexp.sexp,
	      ubool:  bool -> Sexp.sexp,
	      uint:   int  -> Sexp.sexp,
	      ureal:  real -> Sexp.sexp}) =
    (let open Sexp
         open Sxml in
	 let
	     fun name0 s = makename (s,0)

	     fun utype  Nil                 = Lit (SYM SF_NIL)
	       | utype  Unit                = Lit (SYM SF_UNIT)
	       | utype  Bool                = Lit (SYM SF_BOOL)
	       | utype  Int                 = Lit (SYM SF_INT)
	       | utype  Real                = Lit (SYM SF_REAL)
	       | utype  (Field(x,t))        = Seq [Lit (SYM SF_FIELD), 
						   Seq [Lit (SYM SF_NAME), uname (name0 x)],
						   Seq [Lit (SYM SF_TYPE), utype t]]
	       | utype  Record              = Lit (SYM SF_RECORD) 
	       | utype  Pair                = Lit (SYM SF_PAIR)
	       | utype  Vector              = Lit (SYM SF_VECTOR)
	       | utype  Arrow               = Lit (SYM SF_ARROW)
	       | utype  (Tcon (t,ts))       = Seq [Lit (SYM SF_TCON),
						   Seq [Lit (SYM SF_TYPE), utype t],
						   Seq ((Lit (SYM SF_TARGS)) :: (map utype ts))]
	       | utype  (Tvar a)            = Seq [Lit (SYM SF_TVAR), Lit (SYM a)]
	       | utype  (Tabs (a, u))       = Seq [Lit (SYM SF_TABS),
						   Seq ((Lit (SYM SF_TFORMALS)) :: (map (fn (x) => uname (name0 x)) a)),
						   Seq [Lit (SYM SF_TYPE), utype u]]

	     fun uformal (s,t)   = (Seq [Lit (SYM SF_FORMAL),
					 Seq [Lit (SYM SF_NAME), uname (name0 s)], 
					 Seq [Lit (SYM SF_TYPE), utype  t]])

	     fun uenven (s,t,e)   = (Seq [Lit (SYM SF_ENVEN),
					  Seq [Lit (SYM SF_NAME),  uname  (name0 s)], 
					  Seq [Lit (SYM SF_TYPE),  utype  t],
					  Seq [Lit (SYM SF_EXP),   uexp  e]])


	     and uexp  (Const' c)   =
		 (Seq [Lit (SYM SF_CONST),
		       (case c of 
			    Syntax.UNIT_CONST   =>  Lit (SYM SF_UNIT_CONST)
			  | Syntax.NIL_CONST    =>  Lit (SYM SF_NIL_CONST)
			  | Syntax.BOOL_CONST b =>  Seq [Lit (SYM SF_BOOL_CONST), ubool b]
			  | Syntax.REAL_CONST r =>  Seq [Lit (SYM SF_REAL_CONST), ureal r]
			  | Syntax.INT_CONST i  =>  Seq [Lit (SYM SF_INT_CONST), uint i])])
		 
	       | uexp (Var'  s)     = Seq [Lit (SYM SF_VAR), uname (name0 s)]

	       | uexp (If' (e1, e2, e3))   = Seq [Lit (SYM SF_IF),
						 Seq [Lit (SYM SF_E1), uexp e1],
						 Seq [Lit (SYM SF_E2), uexp e2],
						 Seq [Lit (SYM SF_E3), uexp e3]]

	       | uexp (App' (e1, e2))      = Seq [Lit (SYM SF_APP),
						 Seq [Lit (SYM SF_E1), uexp e1],
						 Seq [Lit (SYM SF_E2), uexp e2]]

	       | uexp (Abs' (s, t, e))  = Seq [Lit (SYM SF_ABS),
						  Seq [Lit (SYM SF_FORMAL), 
						       Seq [Lit (SYM SF_NAME), uname (name0 s)],
						       Seq [Lit (SYM SF_TYPE), utype t]],
						  Seq [Lit (SYM SF_EXP), uexp e]]
					     
	       | uexp (Tlam' (a, e))        = Seq [Lit (SYM SF_TLAM),
						  Seq ((Lit (SYM SF_TFORMALS)) :: (map (uname o name0) a)),
						  Seq [Lit (SYM SF_EXP), uexp e]]

	       | uexp (Tapp' (e, ts))       = Seq [Lit (SYM SF_TAPP),
						  Seq [Lit (SYM SF_EXP), uexp e],
						  Seq ((Lit (SYM SF_TARGS)) :: (map utype ts))]
					     
	       | uexp (Tlet' (sts, e))      = Seq [Lit (SYM SF_TLET),
						  Seq ((Lit (SYM SF_TENV)) :: 
						       (map (fn (s,t) => 
								Seq [Lit (SYM SF_TVAL), 
								     Seq [Lit (SYM SF_NAME), (uname (name0 s))],
								     Seq [Lit (SYM SF_TYPE), (utype t)]]) sts)),
						  Seq [Lit (SYM SF_EXP), uexp e]]
					     
	       | uexp (Rcon'    (fs))      = Seq ((Lit (SYM SF_RCON)) ::
						 (map (fn (s,e) => Seq [Lit (SYM SF_FCON),
									Seq [Lit (SYM SF_NAME), uname (name0 s)],
									Seq [Lit (SYM SF_EXP), uexp e]]) fs))
					    
					    
	       | uexp (Rsel'    (e,s))     = Seq [Lit (SYM SF_RSEL),
						 Seq [Lit (SYM SF_FIELD), uname (name0 s)],
						 Seq [Lit (SYM SF_EXP), uexp e]]
					    
	       | uexp (Vcon'    (es))      = Seq ((Lit (SYM SF_VCON)) :: (map uexp es))

	       | uexp (Vref'    (e,i))     = Seq [Lit (SYM SF_VREF),
						 Seq [Lit (SYM SF_EXP), uexp e],
						 Seq [Lit (SYM SF_INDEX), uexp i]]

	       | uexp (Cond    (es,default))   = Seq [Lit (SYM SF_COND),
						      Seq [Lit (SYM SF_DEFAULT), uexp default],
						      Seq ((Lit (SYM SF_CLAUSES)) ::
							   (map (fn(e1,e2) => 
								   Seq [Lit (SYM SF_CLAUSE),
									Seq [Lit (SYM SF_E1), uexp e1],
									Seq [Lit (SYM SF_E2), uexp e2]]) es))]
                                                     
	       | uexp (Lambda  (formals,e)) = Seq [Lit (SYM SF_LAMBDA),
						   Seq ((Lit (SYM SF_FORMALS)) :: (map uformal formals)),
						   Seq [Lit (SYM SF_EXP), uexp e]]

	       | uexp (Let  (env,e)) = Seq [Lit (SYM SF_LET),
					    Seq ((Lit (SYM SF_ENV)) :: (map uenven env)),
					    Seq [Lit (SYM SF_EXP), uexp e]]

					    
	     fun utoplevel (Exp' e)      =  Seq [Lit (SYM SF_TOPLEVEL),
						Seq [Lit (SYM SF_EXP), uexp e]]
					   
	       | utoplevel (Program' (name, env, typ, e))  =  
		 Seq [Lit (SYM SF_TOPLEVEL),
		      Seq [Lit (SYM SF_PROGRAM), 
			   Seq [Lit (SYM SF_NAME), uname (name0 name)],
			   Seq ((Lit (SYM SF_ENV) :: 
				 (map (fn(s,t,e) => 
					 Seq [Lit (SYM SF_VAL),
					      Seq [Lit (SYM SF_NAME), uname (name0 s)],
					      Seq [Lit (SYM SF_TYPE), utype t],
					      Seq [Lit (SYM SF_EXP), uexp e]]) env))),
			   Seq [(Lit (SYM SF_TYPE)), utype typ],
			   Seq [Lit (SYM SF_EXP), uexp e]]]
	 in
	     utoplevel
	 end
    end)


fun parse ({pname:  Sexp.sexp -> name,
	    pbool:  Sexp.sexp -> bool,
	    pint:   Sexp.sexp -> int,
	    preal:  Sexp.sexp -> real}) =
    (let open Sexp
         open Sxml in
	 let
	     fun name0 s = makename (s,0)

	     fun ptype  (x as Seq ((Lit (SYM (_,s))) :: r)) =
		 (fcond [(s=T_SF_NIL,     fn() => Nil),
			 (s=T_SF_UNIT,    fn() => Unit),
			 (s=T_SF_BOOL,    fn() => Bool),
			 (s=T_SF_INT,     fn() => Int),
			 (s=T_SF_REAL,    fn() => Real),
			 (s=T_SF_PAIR,    fn() => Pair),
			 (s=T_SF_ARROW,   fn() => Arrow),
			 (s=T_SF_RECORD,  fn() => Record),
			 (s=T_SF_VECTOR,  fn() => Vector),
			 (s = T_SF_FIELD,
		       fn() => let val name = svassq SF_NAME (Seq r)
				   val typ  = svassq SF_TYPE (Seq r)
			       in
				   Field (symofname (pname name), ptype typ)
			       end),
			 (s = T_SF_TCON,
		       fn() => let val typ    = svassq SF_TYPE (Seq r)
				   val targs  = vassq SF_TARGS (Seq r)
			       in
				   Tcon (ptype typ, map ptype targs)
			       end),
			 (s = T_SF_TVAR,
		       fn() => let val v = case r of [v] => v 
						   | _   => raise ParserError before print "sf: error in ptype\n"
			       in
				   Tvar (symofname (pname v))
			       end),

			 (s = T_SF_TABS,
		       fn() => let val vars = vassq SF_TFORMALS (Seq r)
				   val typ  = svassq SF_TYPE (Seq r)
			       in
				   Tabs (map (fn x => symofname (pname x)) vars,  ptype typ)
			       end)])

	       | ptype _     =  raise ParserError before print "sf: error in ptype\n"

	     fun pconst (x as Seq ((Lit (SYM (_,s))) :: _)) =
		 fcond [(s = T_SF_UNIT_CONST, fn() => UNIT_CONST),
			(s = T_SF_NIL_CONST,  fn() => NIL_CONST),
			(s = T_SF_BOOL_CONST, fn() => BOOL_CONST (pbool (valOf (kid x)))),
			(s = T_SF_REAL_CONST, fn() => REAL_CONST (preal (valOf (kid x)))),
			(s = T_SF_INT_CONST,  fn() => INT_CONST (pint (valOf (kid x))))]
	       | pconst _  = raise ParserError before print "sf: error in pconst\n"

	     fun ptval (Seq ((Lit (SYM (_,x))) :: r)) =
		 fcond [(x = T_SF_TVAL,
		      fn() => let val name = svassq SF_NAME (Seq r) 
				  val typ  = svassq SF_TYPE (Seq r) 
			      in (symofname (pname name), ptype typ) end)]
	       | ptval _ = raise ParserError before print "sf: error in ptval\n"

	     fun pfcon (Seq ((Lit (SYM (_,x))) :: r)) =
		 fcond [(x = T_SF_FCON,
			 fn() => let  val name = svassq SF_NAME (Seq r) 
				      val exp  = svassq SF_EXP (Seq r) 
				 in (symofname (pname name), pexp exp) end)]
	       | pfcon _ = raise ParserError before print "sf: error in pfcon\n"

	     and pformal (x as Seq ((Lit (SYM (_,s)) :: r))) =
		 (fcond [(s=T_SF_FORMAL,
		       fn() => (let val name       = svassq  SF_NAME (Seq r)
				    val typ        = svassq  SF_TYPE (Seq r)
				in (symofname (pname name), ptype typ) end))])
	       | pformal _ = raise ParserError before print "sf: pformal error\n"

	     and pclause (Seq ((Lit (SYM (_,s))) :: r)) =
		 fcond [(s = T_SF_CLAUSE,
		      fn() => let val e1 =  svassq SF_E1 (Seq r)
				  val e2 =  svassq SF_E2 (Seq r)
			      in
				  (pexp e1, pexp e2)
			      end)]
	       | pclause _ =  raise ParserError before print "sf: pclause error\n"

	     and pval (x as Seq ((Lit (SYM (n,s))) :: r)) =
		 (fcond [(s = T_SF_VAL,
		       fn() => let val name = svassq SF_NAME (Seq r) 
				   val typ  = svassq SF_TYPE (Seq r) 
				   val exp  = svassq SF_EXP (Seq r) 
			       in (symofname (pname name), ptype typ, pexp exp) end)])
	       | pval _ = raise ParserError before print "sf: error in pval\n"

	     and pexp (x as Seq ((Lit (SYM (_,s))) :: r))  =
		 (fcond [(s = T_SF_CONST, 
		       fn() => let val v = case r of [v] => v
						   | _   => raise ParserError before print "sf: error in pexp\n"
			       in
				   Const' (pconst v)
			       end),
			 
			 (s = T_SF_VAR,
		       fn() => let val v = case r of [v] => v
						   | _   => raise ParserError before print "sf: error in pexp\n"
			       in
				   Var' (symofname (pname v))
			       end),
			 
			 (s = T_SF_IF,
		       fn() => let val e1 =  svassq SF_E1 (Seq r)
				   val e2 =  svassq SF_E2 (Seq r)
				   val e3 =  svassq SF_E3 (Seq r)
			       in
				   If' (pexp e1, pexp e2, pexp e3)
			       end),
			 
			 (s = T_SF_APP,
		       fn() => let val e1 =  svassq SF_E1 (Seq r)
				   val e2 =  svassq SF_E2 (Seq r)
			       in
				   App' (pexp e1, pexp e2)
			       end),
			 
			 (s = T_SF_ABS,
		       fn() => let val formal =  vassq SF_FORMAL (Seq r)
				   val name   =  svassq SF_NAME (Seq formal)
				   val typ    =  svassq SF_TYPE (Seq formal)
				   val exp    =  svassq SF_EXP (Seq r)
			       in
				   Abs' (symofname (pname  name), 
					 ptype typ,
					 pexp exp)
			       end),
			 
			 (s = T_SF_TLAM,
		       fn() => let val tformals   =  vassq SF_TFORMALS (Seq r)
				   val exp        =  svassq SF_EXP (Seq r)
			       in
				   Tlam' (map (symofname o pname) tformals, pexp exp)
			       end),
			 
			 (s = T_SF_TAPP,
		       fn() => let val exp   =  svassq SF_EXP (Seq r)
				   val targs =  vassq SF_TARGS (Seq r)
			       in
				   Tapp' (pexp exp, map ptype targs)
			       end),
			 
			 (s = T_SF_TLET,
		       fn() => let val tenv   =  vassq SF_TENV (Seq r)
				   val exp    =  svassq SF_EXP (Seq r)
			       in
				   Tlet' (map  (fn x => ptval x) tenv,
					  pexp exp)
			       end),
			 
			 (s = T_SF_RCON,
		       fn() => (Rcon' (map  (fn x =>  pfcon x)  r))),
			 
			 (s = T_SF_RSEL,
		       fn() => let val field   =  svassq SF_FIELD (Seq r)
				   val exp    =  svassq SF_EXP (Seq r)
			       in
				   Rsel' (pexp exp, symofname (pname field))
			       end),
			 
			 (s = T_SF_VCON,
		       fn() => (Vcon' (map  (fn x => pexp x)  r))),
			 
			 (s = T_SF_VREF,
		       fn() => let val exp    =  svassq SF_EXP (Seq r)
				   val index  =  svassq SF_INDEX (Seq r)
			       in
				   Vref' (pexp exp, pexp index)
			       end),

			 (s = T_SF_COND,
		       fn() => let val default = svassq SF_DEFAULT (Seq r)
				   val clauses = vassq SF_CLAUSES (Seq r)
			       in 
				   Cond (map pclause clauses, pexp default)
			       end),
			 
			 (s = T_SF_LAMBDA,
		       fn() => let val formals = vassq SF_FORMALS (Seq r)
				   val exp    = svassq SF_EXP (Seq r)
			       in 
				   Lambda (map pformal formals, pexp exp)
			       end),
			 
			 (s = T_SF_LET,
		       fn() => let val env   = vassq SF_ENV (Seq r)
				   val exp   = svassq SF_EXP (Seq r)
			       in 
				   Let (map pval env, pexp exp)
			       end)

		 ])
		     
	       | pexp x  = raise ParserError before (print "sf: error in pexp: x = "; 
						     Sexp.pp x; print "\n")


	     fun ptoplevel (Seq [Lit (SYM (_,x)), Seq [Lit (SYM (_,y)), exp]]) =
		 fcond [(x=T_SF_TOPLEVEL andalso y=T_SF_EXP,
			 fn() => Exp' (pexp exp))]

	       | ptoplevel  (Seq [Lit (SYM (sx,x)), Seq ((Lit (SYM (sy,y))) :: r)]) =
		 let val _ = print ("ptoplevel: x = " ^ (sx) ^ "\n")
		     val _ = print ("ptoplevel: y = " ^ (sy) ^ "\n")
		     val _ = print ("ptoplevel: (x=T_SF_TOPLEVEL andalso y=T_SF_PROGRAM) = " ^
				    (Bool.toString (x=T_SF_TOPLEVEL andalso y=T_SF_PROGRAM)) ^ "\n")
		 in
		     fcond [(x=T_SF_TOPLEVEL andalso y=T_SF_PROGRAM,
			  fn() => let val name = symofname (pname (svassq SF_NAME (Seq r)))
				      val _    = print ("ptoplevel: name = " ^ (Symbol.name name) ^ "\n")
				      val env  = map pval (vassq SF_ENV (Seq r))
				      val _    = print ("ptoplevel: env done \n")
				      val typ  = ptype (svassq SF_TYPE (Seq r))
				      val _    = print ("ptoplevel: type done \n")
				      val exp  = svassq SF_EXP (Seq r)
				      val _    = (print "ptoplevel: exp = "; Sexp.pp exp; print "\n")
				  in Program' (name, env, typ, pexp exp)
				  end)]
		 end

	       | ptoplevel _  = raise ParserError before print "sf: error in ptoplevel\n"
	 in
	     ptoplevel
	 end
    end)


val toSexp: toplevel' -> Sexp.sexp = 
	(unparse (let open Sexp in
		      ({uname   = (fn n: name   => Lit (SYM (symofname n))),
			ubool   = (fn b: bool   => Lit (SYM (case b of true => Symbol.symbol "#true"
								     | false => Symbol.symbol "#false"))),
			uint    = (fn i: int    => Lit (INT i)),
			ureal   = (fn r: real   => Lit (REAL r))})
		  end))

val toSXML: toplevel' -> Sexp.sexp = 
    (unparse (let open Sxml
	      in
		  {uname=uname, ubool=ubool, uint=uint, ureal=ureal}
	      end))
    


val fromSexp: Sexp.sexp -> toplevel' =
    (parse    (let open Sexp in 
		   {pname   =  (fn (Lit (SYM s))      => makename (s, 0)
				 | _            => raise ParserError),
		    pbool   =  (fn (Lit (SYM x))      => Symbol.equal (x, Symbol.symbol "#true")
				 | _            => raise ParserError),
		    pint    =  (fn (Lit (INT i))      => i
				 | _            => raise ParserError),
		    preal   =  (fn (Lit (REAL r))      => r
				 | _            => raise ParserError)}
	       end))



val fromSXML: Sexp.sexp -> toplevel' = 
    let val parse'     = parse
	val  (_,t)     = Symbol.symbol "*TOP*"
	open Sexp 
	open Sxml
    in
	fn (Seq ((Lit (SYM (_,s))) :: x)) =>
	   (print "entering fromSXML\n";
	   if s = t
	   then
	       (let val _     = print "fromSXML: toplevel\n"
		    val model = case (assq SF_TOPLEVEL (Seq  x)) of 
				    SOME x  => x
				  | NONE    => 
				    raise ParserError  
 		in
		    parse' ({pname=pname, pbool=pbool, pint=pint, preal=preal}) model
		end)
	   else raise ParserError before print "sf: error in fromSXML\n")

	 | _ => raise ParserError before print "sf: error in fromSXML\n"
    
    end

end
