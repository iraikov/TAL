(* 
          _Strictly Pretty_
	  Christian Lindig
*)

signature PRETTY =
sig

    datatype doc =     DocNil
		     | DocCons   of doc * doc
		     | DocText   of string
		     | DocNest   of int * doc
		     | DocBreak  of string
		     | DocGroup  of doc

    val empty : unit          -> doc
    val text  : string        -> doc 
    val nest  : int * doc     -> doc
    val cons  : doc * doc     -> doc
    val break : unit          -> doc
    val breakWith  : string   -> doc
    val group   : doc         -> doc
    val connect : doc * doc   -> doc
    val connectWith : string * doc * doc   -> doc
    val concat : doc list -> doc

    datatype delim = L | R

    val list    : int -> (('a -> doc) * (unit -> doc)) -> 'a list -> doc
    val binop   : int -> doc * doc * doc -> doc
    val ifthen  : {i: doc, t: doc, e: doc} -> int -> doc * doc * doc -> doc
    val block   : {l: doc, r: doc} -> int -> doc -> doc
    val letblk  : {l: doc, i: doc, e: doc} -> int -> doc * doc -> doc
    val procblk : {h: doc, l: doc, r: doc} -> int -> doc -> doc

    val space   : unit -> doc
    val comma   : unit -> doc
    val paren   : delim -> doc
    val angle   : delim -> doc
    val bracket : delim -> doc
    val brace   : delim -> doc

				
    datatype sdoc =  SNil
		   | SText  of string * sdoc
		   | SLine  of int    * sdoc

    datatype mode = Flat | Break			       

    val output    : TextIO.outstream -> string -> unit

    val format    : int -> doc -> sdoc

    val toString  : sdoc -> string

    val printf    : TextIO.outstream -> sdoc -> unit
end



structure Pretty :> PRETTY =
struct


    datatype doc =     DocNil
		     | DocCons   of doc * doc
		     | DocText   of string
		     | DocNest   of int * doc
		     | DocBreak  of string
		     | DocGroup  of doc
				
    datatype sdoc =  SNil
		   | SText  of string * sdoc
		   | SLine  of int    * sdoc

    datatype mode = Flat | Break			       

    datatype delim = L | R


    fun cons (x,y)   = DocCons(x,y)
    fun empty ()     = DocNil
    fun text s       = DocText(s)
    fun nest (i,x)   = DocNest(i,x)
    fun break ()     = DocBreak(" ")
    fun breakWith s  = DocBreak(s)
    fun group d      = DocGroup(d)


    fun connect (x,y) = case (x,y) of (DocNil,_) => y
				    | (_,DocNil) => x
				    | (_,_)      => cons (x, cons(break(),y))

    fun connectWith (s,x,y) = case (x,y) of (DocNil,_) => y
					  | (_,DocNil) => x
					  | (_,_)      => cons (x, cons(breakWith s,y))
							  
    fun  concat []           = empty()
       | concat (x::[])      = group x
       | concat (DocNil::r)  = concat r
       | concat (x::r)       = cons(x,concat r)

	
    fun fits w x =
	(if (w < 0) then false 
	 else
	     (case x of
		  []                          => true
		| (i,m,DocNil)           :: z => fits w z
		| (i,m,DocCons(x,y))     :: z => fits w ((i,m,x)::(i,m,y)::z)
		| (i,m,DocNest(j,x))     :: z => fits w ((i+j,m,x)::z)
		| (i,m,DocText(s))       :: z => fits (w - (String.size s)) z
		| (i,Flat,DocBreak(s))   :: z => fits (w - (String.size s)) z
		| (i,Break,DocBreak(s))  :: z => true
		| (i,m,DocGroup(x))      :: z => fits w ((i,Flat,x)::z)))
	
    fun format' w k x = 
	(case x of
	     []                         => SNil
	   | (i,m,DocNil)          :: z => format' w k z
	   | (i,m,DocCons(x,y))    :: z => format' w k ((i,m,x)::(i,m,y)::z)
	   | (i,m,DocNest(j,x))    :: z => format' w k ((i+j,m,x)::z)
	   | (i,m,DocText(s))      :: z => SText(s,format' w (k + (String.size s)) z)
	   | (i,Flat,DocBreak(s))  :: z => SText(s,format' w (k + (String.size s)) z)
	   | (i,Break,DocBreak(s)) :: z => SText(s,SLine(i,format' w i z))
	   | (i,m,DocGroup(x))     :: z => (if fits (w-k) ((i,Flat,x)::z)
					    then format' w k ((i,Flat,x)::z)
					    else format' w k ((i,Break,x)::z)))
						    
    fun format w x = format' w 0 ([(0,Flat,DocGroup x)])
		     
    fun toString x = 
	(case x of  
	     SNil        => ""
	   | SText(s,d)  => s ^ (toString d)
	   | SLine(i,d)  => 
	     let 
		 val prefix = String.implode(List.tabulate(i,fn(i) => #" "))
	     in  
		 "\n" ^ prefix ^ (toString d)
	     end)

    fun output strm x = TextIO.outputSubstr (strm,Substring.full x)

    fun printf strm x =
	(case x of  
	     SNil        => ()
	   | SText(s,d)  => (output strm s; printf strm d)
	   | SLine(i,d)  => 
	     let 
		 val prefix = String.implode(List.tabulate(i,fn(i) => #" "))
	     in  
		 output strm ("\n" ^ prefix); printf strm  d
	     end)

    fun binop ind (left,oper,right) = group (nest (ind, (connect (group (connect (left, oper)), right))))


    fun list ind (ppElem,ppSep) l = 
	(let
	     fun ppl' ax [x]     = rev ((group (nest (ind, (ppElem x)))) :: ax)
	       | ppl' ax (x::r)  = ppl' ((ppSep ()) :: (group (nest (ind, (ppElem x)))) :: ax) r
	       | ppl' ax []      = rev ax
	 in
	     group(concat (ppl' [] l))
	 end)

    fun ifthen {i,t,e} = 
	(fn ind => 
	 fn (c,e1,e2) =>
	    group (nest (2, (connect ((connect (i, c)),
				      connect (group (nest (ind, (connect (t, e1)))),
					       group (nest (ind, (connect (e, e2))))))))))
    fun block {l,r} = 
	(fn ind => 
	 fn b => (group (cons (l, cons (nest (ind, b), r)))))

    fun letblk {l,i,e} = 
	(fn ind => 
	 fn (e1,e2) =>
	    group (connect (nest (ind, connect (l, group e1)),
			    connect (nest (ind, connect (i, group e2)), e))))
	
    fun procblk {h,l,r} = 
	(fn ind => fn (e) => group (connect (group (connect (h, nest (ind, connect (l, group e)))), r)))
			    
	

    fun space ()   = (text " ")
		     
    fun comma ()   = (breakWith ", ")
		     
    fun paren L     = text "(" 
      | paren R     = text ")"

    fun angle L     = text "<" 
      | angle R     = text ">"

    fun bracket L   = text "[" 
      | bracket R   = text "]"

    fun brace L     = text "{" 
      | brace R     = text "}"

(*
    val cond = binop 2 (text "a", text "==", text "b")
    val e1   = binop 2 (text "a", text "<<", text "2")
    val e2   = binop 2 (text "a", text "+", text "b")


    val doc1 = ifthen {i=text "if",t=text "then",e=text "else"} 2 (cond,e1,e2)

    val doc2 = block {l=paren L, r=paren R} 2 doc1

    val doc3 = list 2 (fn x => x,break) [e1,e2]

    val doc4 = letblk {l=text "program", i=text "in", e=text "end"} 2  (doc3, doc1)
	       
    val _   = print ((toString (format 32 doc4)) ^ "\n")
    val _   = print ((toString (format 10 doc4)) ^ "\n")
*)

end

