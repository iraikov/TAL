
(* 
 * Sugar.sml
 *
 * System F syntactic sugar.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)



structure Sugar : SUGAR =
struct

open Type

datatype sugarexp = Const'  of const
		  | Var'    of symbol
		  | If'     of sugarexp * sugarexp * sugarexp
		  | App'    of sugarexp * sugarexp
		  | Abs'    of symbol * typ * sugarexp
		  | Tlam'   of symbol list * sugarexp
		  | Tapp'   of sugarexp * typ list
		  | Tlet'   of (symbol * typ) list * sugarexp
		  | Rcon'   of (symbol * sugarexp) list
		  | Rsel'   of sugarexp * symbol  
		  | Vcon'   of sugarexp list
		  | Vref'   of sugarexp * sugarexp

		  | Cond    of (sugarexp * sugarexp) list * sugarexp
		  | Lambda  of (symbol * typ) list * sugarexp
		  | Let     of (symbol * typ * sugarexp) list * sugarexp

			       
type program' = symbol * (symbol * typ * sugarexp) list * typ * sugarexp

datatype toplevel' = Exp' of sugarexp
		   | Program' of program'


fun ecompose (a,v,T1) = 
    let 
	fun compose1 (a,v,T1) =
	    let 
		val a' = Env.find (a,T1)
	    in
		case a' of 
		    SOME a' => 
		    let
			val T2  = Env.enter (a,Var v,Env.empty)
			val T1' = Env.map (fn(t) => esubst T2 t) T1
			val T1' = Env.enter (a, Var v, T1')
		    in
			Env.enter (v,a',T1')
		    end
		  | NONE    => Env.enter (a,Var v,T1)
	    end
    in
	ListPair.foldl compose1 T1 (a,v)
    end


and esubst T e =
    (case e of 
	 (Var a) => (case Env.find (a, T) of SOME t => t | NONE   => Var a)
       | (Abs (x,t,e)) =>
	 (let
	      val x'      = Symbol.freshPrefix "x"
	      val T'      = ecompose ([x],[x'],T)
	  in
	      Abs(x',t,esubst T' e)
	  end)
       | Let1 ((x,t,e1),e2) =>
	 (let
	      val x'      = Symbol.freshPrefix "x"
	      val T'      = ecompose ([x],[x'],T)
	  in
	      Let1((x',t,esubst T e1),esubst T' e2)
	  end)
       | App (e1,e2)   => App (esubst T e1, esubst T e2)
       | If (e1,e2,e3) => If (esubst T e1,esubst T e2,esubst T e3)
       | Tapp (e,ts)   => Tapp (esubst T e, ts)
       | Tlet (sts,e)  => Tlet (sts, esubst T e)
       | Tlam (ss,e)   => Tlam (ss, esubst T e)
       | Rcon fs       => Rcon (map (fn(s,e) => (s,esubst T e)) fs)
       | Rsel (e,s)    => Rsel (esubst T e, s)
       | Vcon es       => Vcon (map (esubst T) es)
       | Vref (e1,e2)  => Vref (esubst T e1, esubst T e2)
       | Texp (t,e)    => Texp (t, esubst T e)
       | Tprim (t,e)   => Tprim (t, esubst T e)
       | Const _       => e
       | Tbox _        => e)


fun transform (Const' c)             = Const c
  | transform (Var' s)               = Var s
  | transform (If'  (e1, e2, e3))    = If (transform e1, transform e2, transform e3)
  | transform (App' (e1, e2))        = App (transform e1, transform e2)
  | transform (Abs' (s,t,e))         = Abs (s,t,transform e)
  | transform (Tapp' (e, ts))        = Tapp (transform e, ts)
  | transform (Tlam' (a, e))         = Tlam (a, transform e)
  | transform (Tlet' (ts, e))        = Tlet (ts, transform e)
  | transform (Rcon' ses)            = Rcon (map (fn(s,e) => (s, transform e)) ses)
  | transform (Rsel' (e,s))          = Rsel (transform e,s)
  | transform (Vcon' (es))           = Vcon (map (transform) es)
  | transform (Vref' (e,i))          = Vref (transform e, transform i)
  | transform (Cond (conds, default))  = transform_cond (transform default) conds
  | transform (Lambda (args, e))       = transform_lambda (transform e) args
  | transform (Let (vals, e))          = transform_let (transform e) vals

(* N-way branch *)
and transform_cond default ([]) = default
  | transform_cond default ((t,e)::rest) =
    If (transform t, transform e, transform_cond default rest)

(* Multi-argument non-recursive unnamed function *)
and transform_lambda body ([]) = 
    Abs (Symbol.fresh(), Unit, body)
  | transform_lambda body ((n,t)::[]) =
    Abs (n,t,body)
  | transform_lambda body ((n,t)::rest) =
    Abs (n,t,(transform_lambda body rest))

(* Local bindings *)
and transform_let body ((s,t,e)::rest) = 
    Let1 ((s,t,transform e),transform_let body rest)
  | transform_let body ([]) = body


fun transform_program (p, env, ty, body) =
    let
	val root = Symbol.freshPrefix "rootws"

	val env'  = foldr (fn((s,t,e), env) => (s,t,esubst Env.empty (transform e))::env) [] env
	val body' = esubst Env.empty (transform body)
    in
	(p, env', ty, body')
    end


end
