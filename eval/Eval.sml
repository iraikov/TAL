
(* 
 * Eval.sml
 *
 * A simple interpreter for System F -- evaluation.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)


structure Eval : EVAL =
struct
    open SF
    open Value


    fun eval V (Const c) =    constant c
      | eval V (Var x) =
	    (Env.look(x,V) handle Env.Unbound _ => 
                                  (raise ValueError 
                                   before print ("eval Var [" ^ (Symbol.name x)^ "]\n")))

      | eval V (x as If (e1,e2,e3)) =
	    (case eval V e1
	      of  BoolVal true  => eval V e2
		| BoolVal false => eval V e3
		| _     => raise ValueError 
				 before (print "If: x = ";
					 (pp (Exp x))))

      | eval V (x as App (e1,e2)) =
	let
	    val v1 = eval V e1
	    val v2 = eval V e2
	in
	    case v1
	     of 
		Op oper               => applyOp (oper,v2)
	      | Proc (x,e,V')         => eval (Env.enter (x,v2,V')) e
	      | RecProc (x1,x2,e,V')  => eval (Env.enter (x2,v2, Env.enter (x1,v1,V'))) e
	      | _ => raise ValueError  before (print "App: x = ";
					       (pp (Exp x)))
	end
      | eval V (Abs (x,t,e))    =   Proc (x,e,V)
      | eval V (x as Tbox (s,tbs,t)) =  let val v = Env.look(s,V) 
						handle Env.Unbound _ => 
						       raise ValueError before (print "Tbox: x = ";
										(pp (Exp x)))
					in  v  end
 
      | eval V (Rcon ses)       =  let val (ss, es) = ListPair.unzip ses
				       val enter = (fn(s,e,R) => Env.enter(s,eval V e,R))
				   in
				       RecordVal (ss, ListPair.foldl enter  Env.empty (ss,es))
				   end
      | eval V (x as Rsel (e,s))     = let val rd = eval V e
				       in case rd of RecordVal (_,env) => Env.look (s, env)
						   | _ => raise ValueError before (print "Rsel: x = ";
										   (pp (Exp x)))
				       end


      | eval V (x as Vref (e,i))     = let val v = eval V e
					   val i = case eval V i of
						       IntNum i   => i
						     | _          => raise ValueError
									   before (print "Vref: x = ";
										   (pp (Exp x)))
				  in
				      case v of VectorVal v  => Vector.sub (v,i)
					      | _            => raise ValueError
								      before  (print "Vref: x = ";
									       (pp (Exp x)))
				  end
				   
      | eval V (Vcon (es))      = let  val v = Vector.fromList (map (fn(e) => eval V e) es)
				  in VectorVal v end
				  
      | eval V (Texp(t,e))      = eval V e

      | eval V x                = raise ValueError before (print "eval: x = ";
							   (pp (Exp x)))


    fun evalProgram V (p, env, t, e) =
	let
	    val V' = foldl (fn((s,t,e), V') => Env.enter (s,eval V e,V')) V env
	in
	    eval V' e
	end

end
