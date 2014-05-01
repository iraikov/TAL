
(* 
 * Value.sml
 *
 * A simple interpreter for System F -- values
 *
 * Version $Revision: 1.1.1.1 $
 *
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 * 02110-1301 USA
 *
 * A full copy of the GPL license can be found on Debian systems in 
 * /usr/share/common-licenses/GPL-2
 *
 *)

structure Value : VALUE =
struct
    exception ValueError

    type symbol = Symbol.symbol

    type const     = SF.const
    type typ       = SF.typ
    type exp       = SF.exp
    type program   = SF.program
	      
    datatype oper  = Add  | Sub  | Mul  | Div    | Eq  | Less  | Or | Not
		   | IAdd' of int | ISub' of int | IMul' of int | IDiv' of int
		   | IEq' of int | ILess' of int 
         	   | RAdd' of real | RSub' of real | RMul' of real | RDiv' of real
	   	   | REq' of real | RLess' of real 
		   | Math of string
		   | Or' of bool
		   | Cons | Car | Cdr 
		   | ICons of int | RCons of real | BCons of bool | PCons of value * value
		   | PEq of value
		   | RDCons of symbol list * value Env.env 

	 and value = UnitVal
		   | NilVal      
		   | BoolVal     of bool
		   | IntNum      of int
		   | RealNum     of real
		   | PairVal     of value * value
		   | RecordVal   of symbol list * value Env.env 
		   | VectorVal   of value Vector.vector
		   | Op          of oper
		   | Proc        of symbol * exp * value Env.env
		   | RecProc     of symbol * symbol * exp * value Env.env


    fun equal (v1: value, v2: value) =
	(case (v1, v2) of
	     (UnitVal,    UnitVal)              => true
	   | (BoolVal b1, BoolVal b2)           => true
	   | (IntNum i1,  IntNum i2)            => i1=i2
	   | (RealNum r1, RealNum r2)           => Real.compare (r1,r2) = General.EQUAL
	   | (PairVal (l1,r1), PairVal (l2,r2)) => equal(l1,l2) andalso equal(l2,r2)
	   | (_, _)  => false)

    and listEqual (vl1: value list, vl2: value list) = 
	(case (vl1, vl2) of
	     (v1::rest1, v2::rest2) => 
	     if equal (v1, v2)  then listEqual (rest1, rest2)  else false
	   | (nil, nil)             => true
	   | (_, _)                 => false)

    fun constant n = let open SF in 
			 case n of UNIT_CONST       => UnitVal
				 | NIL_CONST        => NilVal 
				 | BOOL_CONST b     => BoolVal b
				 | INT_CONST  i     => IntNum i
				 | REAL_CONST r     => RealNum r
		     end

    fun applyOp (Add,       IntNum n) = Op (IAdd' n)
      | applyOp (Sub,       IntNum n) = Op (ISub' n)
      | applyOp (Mul,       IntNum n) = Op (IMul' n)
      | applyOp (Div,       IntNum n) = Op (RDiv' (Real.fromInt n))
      | applyOp (Eq,        IntNum n) = Op (IEq' n)
      | applyOp (Less,      IntNum n) = Op (ILess' n)
      | applyOp (IAdd' m,   IntNum n) = IntNum (m+n)
      | applyOp (ISub' m,   IntNum n) = IntNum (m-n)
      | applyOp (IMul' m,   IntNum n) = IntNum (m*n)
      | applyOp (IEq' m,    IntNum n) = BoolVal (if m = n then true else false)
      | applyOp (ILess'  m, IntNum n) = BoolVal (if m < n then true else false)

      | applyOp (Add,      RealNum n) = Op (RAdd' n)
      | applyOp (Sub,      RealNum n) = Op (RSub' n)
      | applyOp (Mul,      RealNum n) = Op (RMul' n)
      | applyOp (Div,      RealNum n) = Op (RDiv' n)
      | applyOp (Eq,       RealNum n) = Op (REq' n)
      | applyOp (Less,     RealNum n) = Op (RLess' n)
      | applyOp (RAdd' m,   RealNum n) = RealNum (m+n)
      | applyOp (RSub' m,   RealNum n) = RealNum (m-n)
      | applyOp (RMul' m,   RealNum n) = RealNum (m*n)
      | applyOp (RDiv' m,   IntNum n)  = RealNum (m/(Real.fromInt n))
      | applyOp (RDiv' m,   RealNum n) = RealNum (m/n)
      | applyOp (REq' m,    RealNum n) = BoolVal (if Real.compare (m,n) = General.EQUAL then true else false)
      | applyOp (RLess'  m, RealNum n) = BoolVal (if m < n then true else false)

      | applyOp (Math "round", RealNum n) = IntNum(Real.round(n))
      | applyOp (Math "ln",   RealNum n)  = RealNum(Math.ln(n))
      | applyOp (Math "exp",  RealNum n)  = RealNum(Math.exp(n))
      | applyOp (Math "sqrt", RealNum n)  = RealNum(Math.sqrt(n))
      | applyOp (Math "sin",  RealNum n)  = RealNum(Math.sin(n))
      | applyOp (Math "cos",  RealNum n)  = RealNum(Math.cos(n))
      | applyOp (Math "tan",  RealNum n)  = RealNum(Math.tan(n))
      | applyOp (Math "asin",  RealNum n) = RealNum(Math.asin(n))
      | applyOp (Math "acos",  RealNum n) = RealNum(Math.acos(n))
      | applyOp (Math "atan",  RealNum n) = RealNum(Math.atan(n))
      | applyOp (Math "sinh",  RealNum n) = RealNum(Math.sinh(n))
      | applyOp (Math "cosh",  RealNum n) = RealNum(Math.cosh(n))
      | applyOp (Math "tanh",  RealNum n) = RealNum(Math.tanh(n))

      | applyOp (Or,        BoolVal b)      = Op (Or' b)
      | applyOp (Not,       BoolVal true)   = BoolVal (false)
      | applyOp (Not,       BoolVal false)  = BoolVal (true)

      | applyOp (Eq,       PairVal l)           = Op (PEq (PairVal l))
      | applyOp (Cons,     IntNum n)            = Op (ICons n) 
      | applyOp (Cons,     RealNum n)           = Op (RCons n) 
      | applyOp (Cons,     BoolVal b)           = Op (BCons b) 
      | applyOp (Cons,     PairVal p)           = Op (PCons p) 
      | applyOp (Cons,     RecordVal (ss,rd))   = Op (RDCons (ss,rd))
      | applyOp (Car,      PairVal (l,r))       = l
      | applyOp (Cdr,      PairVal (l,r))       = r

      | applyOp (ICons n,  NilVal)                     = PairVal (IntNum n, NilVal)
      | applyOp (RCons n,  NilVal)                     = PairVal (RealNum n, NilVal) 
      | applyOp (BCons b,  NilVal)                     = PairVal (BoolVal b, NilVal) 
      | applyOp (PCons l1, NilVal)                     = PairVal (PairVal l1, NilVal)
      | applyOp (RDCons (ss1,rd1), NilVal)             = PairVal (RecordVal (ss1,rd1), NilVal)

      | applyOp (ICons n,  p as PairVal (IntNum _, _))  = PairVal (IntNum n, p)
      | applyOp (RCons n,  p as PairVal (RealNum _, _)) = PairVal (RealNum n, p) 
      | applyOp (BCons b,  p as PairVal (BoolVal _, _)) = PairVal (BoolVal b, p) 
      | applyOp (PCons l1, p as PairVal l2)             = PairVal (PairVal l1, p)
      | applyOp (RDCons (ss1,rd1), p as PairVal rdl)    = PairVal (RecordVal (ss1,rd1), p)

      | applyOp (PEq NilVal, NilVal)             = BoolVal (true)
      | applyOp (PEq (PairVal p1), NilVal)       = BoolVal (false)
      | applyOp (PEq (PairVal p1), PairVal p2)   = BoolVal (equal (PairVal p1,PairVal p2))

      | applyOp v                = raise ValueError

    val valbase = List.foldl (fn ((x,v),V) => Env.enter (x,v,V)) Env.empty
			  [(Symbol.symbol "add",   Op Add),
			   (Symbol.symbol "sub",   Op Sub),
			   (Symbol.symbol "mul",   Op Mul),
			   (Symbol.symbol "eq",    Op Eq),
			   (Symbol.symbol "less",  Op Less),
			   (Symbol.symbol "div",   Op Div),
			   (Symbol.symbol "round", Op (Math "round")),
			   (Symbol.symbol "exp",   Op (Math "exp")),
			   (Symbol.symbol "sqrt",  Op (Math "sqrt")),
			   (Symbol.symbol "sin",   Op (Math "sin")),
			   (Symbol.symbol "cos",   Op (Math "cos")),
			   (Symbol.symbol "tan",   Op (Math "tan")),
			   (Symbol.symbol "acos",  Op (Math "acos")),
			   (Symbol.symbol "atan",  Op (Math "atan")),
			   (Symbol.symbol "ln",    Op (Math "ln")),
			   (Symbol.symbol "sinh",  Op (Math "sinh")),
			   (Symbol.symbol "cosh",  Op (Math "cosh")),
			   (Symbol.symbol "tanh",  Op (Math "tanh")),
			   (Symbol.symbol "not",   Op Not),
			   (Symbol.symbol "or",    Op Or),
			   (Symbol.symbol "cons",  Op Cons),
			   (Symbol.symbol "car",   Op Car),
			   (Symbol.symbol "cdr",   Op Cdr)]



end

(*
 * $Id: Value.sml,v 1.1.1.1 2006/06/19 19:40:34 ivan_raikov Exp $
 *
 * 
 * $Log: Value.sml,v $
 * Revision 1.1.1.1  2006/06/19 19:40:34  ivan_raikov
 * Imported sources
 *
 *
 *)
