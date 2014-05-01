
(* 
 * Bloom.sml
 *
 * Hashtable implemented with a counting Bloom filter.
 * Based on the Shared-node Fast Hash Table (SFHT) data structure described  
 * by Song, et al., in _Fast Hash Table Lookup Using Extended Bloom Filter:
 *  An Aid to Network Processing_. (SIGCOMM'05)
 *
 * 
 * Version $Revision: 1.4 $
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
 *
 *)


signature SFHT =
sig
    eqtype key

    type 'a map

    val empty  : int * real -> 'a map
    val insert : key * 'a * 'a map -> 'a map
    val remove : key * 'a map -> 'a map
    val find   : key * 'a map -> 'a option

				 
    val listBkts  : 'a map -> (int * int * ((key * 'a) list)) list
end



functor SfhtFn (eqtype key
		val equal: key * key -> bool
                val getInt: key -> int) : SFHT =
struct

    exception InvalidInplaceAppend

    structure A = DynArray

    type key = key

    datatype 'a bucket = BktNil | BktCons of (key * 'a) * ('a bucket ref) 

    type word = Word.word

    type 'a bentry = (word ref) * ('a bucket) ref

    type 'a bstate = {
         n:  int,  (* capacity *)
	 p:  real, (* false positive rate *)
	 m:  int,  (* number of bits in vector *)
	 k:  int,  (* number of hash functions *) 
	 ba: ('a bentry) A.array
    }

    type 'a map = 'a bstate

    fun bktList'  (BktNil, ax)  = rev ax
      | bktList'  (BktCons ((k,x),ref l), ax) = bktList' (l,(k,x) :: ax)
						
    fun bktList b = bktList' (b,[])
		    
    fun cons (x,b) = BktCons(x,b)

    (* in place append: append to list without creating a new copy *)
    fun iappend (BktNil, b2)  = raise InvalidInplaceAppend
      | iappend (BktCons (_,bl as ref BktNil),b2) = (bl := b2)
      | iappend (BktCons (_,ref bl),b2) = iappend (bl,b2)
				

    (* Hash function based on uniform pseudo-random numbers *)
    fun hash (key,k,n,ins) = 
	let 
	    open Random

	    val sub  = BitArray.sub

	    val h    = getInt key

	    val seed = rand(Int.mod(h,n),h)
	       
	    fun rng (s) = randRange (0,n-1) s
			  
	    fun loop (i,s,ax) = 
		if i > 0 
		then (let val v = rng s
		      in loop (i-1,s,(v::ax)) end)
		else rev ax
		     
	in 
	    loop (k,seed,[]) 
	end
			 

    fun empty (n: int, p) =
       let 
	   val m  = Real.round (1.0 + (Real.fromInt n) * Math.ln(p) / Math.ln(0.6185))
	   val k  = Real.round (0.5 + Math.ln(p) / Math.ln(0.5))

	   val default = (ref 0w0, ref BktNil)

	   val ba = A.array (m, default)

       in 
	   {n=n, p=p, m=m, k=k, ba=ba}
       end


   fun insert (key,x,bf as {n,p,m,k,ba}) =
       let open Word
	   val h = hash(key,k,n,true)

	   val b = cons ((key,x),ref BktNil)
		   
	   fun tail (l, x as ref BktNil)  = x
	     | tail (l, x as ref (BktCons(_,r))) = 
	       (if l>0w0 then tail (l-0w1,r) else x)

	   fun loop i =
	       let 
		   val (sz,bkt) = A.sub(ba,i)
	       in 
		   (if (!sz) = 0w0
		    then (A.update(ba,i,(ref 0w1, ref b)))
		    else (case tail ((!sz),bkt) of
			      (* in-place append item to the list *)
			      ref BktNil    =>  iappend (!bkt,b)
			      (* prepend item to the list *)
			    | ref (BktCons _) =>  bkt := cons((key,x),ref (!bkt));
			  sz := (!sz) + 0w1))
	       end

       in
	   app loop h; bf
       end

       
   fun remove (key,bf as {n,p,m,k,ba}) =
       let open Word
	   val h = hash(key,k,n,false)

	   fun loop1 i =
	       let 
		   val (sz,bkt) = A.sub(ba,i)

		   fun loop2 (l, ref BktNil)  = ()
		     | loop2 (l, x as ref (BktCons((key',_),r as ref y))) = 
		       if l>0w0 then (if equal (key,key') then x := y else loop2 (l-0w1,r))
		       else ()
		       
	       in 
		   loop2 ((!sz),bkt);

		   sz := (!sz) - 0w1
	       end

       in
	   app loop1 h; bf
       end


   fun find (key,bf as {n,p,m,k,ba}) =
       let open Word
	   val h   = hash(key,k,n,false)
	   val cbs = map (fn i => A.sub(ba,i)) h

	   fun minc ((sz,bkt),(n,b)) = 
	       if (!sz) < (!n) then (sz,bkt) else (n,b)

	   val k    = getInt key

	   fun loop (0w0, x)         = NONE
	     | loop (l, ref BktNil)  = NONE
	     | loop (l, ref (BktCons((key',x),r))) = 
	       if equal (key,key') then SOME x else loop (l-0w1,r)

	   val (sz,bkt) = foldl minc (hd cbs) (tl cbs)

	   val x = loop (!sz,bkt)
       in
	   x
       end

   fun listBkts (bf as {n,p,m,k,ba}) =
       let open Word
	   fun loop (i,(sz,bkt),ax) = 
	       if (!sz) > 0w0
	       then ((i,Word.toInt (!sz),bktList (!bkt)) :: ax)
	       else ax
       in 
	   A.foldli loop [] ba
       end

(*

   fun test ()  = 
       (let 
	    open Word8Vector

	    val h  = empty (10000, 0.0001)

	    val k1 = hashWord8Vector (fromList [0w1,0w2,0w3,0w4])
	    val k2 = hashWord8Vector (fromList [0w5,0w6,0w7,0w8])

	    val h = insert (k1, "a", h)	
	    val h = insert (k2, "b", h)
	    val h = insert (k2, "bb", h)

		    
	    val concat = String.concat
			 
	    fun ppList (ppElem,ppSep) l = 
		let
		    fun ppl' ax []      = rev ax
		      | ppl' ax [x]     = rev ((ppElem x) :: ax)
		      | ppl' ax (x::r)  = ppl' ((ppSep()) :: (ppElem x) :: ax) r
		in
		    ppl' [] l
		end
		
		
	    fun ppBkt (i,sz,l) = concat ["bucket #", (Int.toString i), " ",
					 "sz=", Int.toString sz, " ", "els=", 
					 concat (ppList (fn (k,x) => x, fn()=>",") l)]
				 
	    val bkts = listBkts h
	in 
	    print ("n = " ^ (Int.toString (#n h)) ^ "\n");
	    print ("m = " ^ (Int.toString (#m h)) ^ "\n");
	    print ("k = " ^ (Int.toString (#k h)) ^ "\n");
	    print ("# buckets: " ^ (Int.toString (length bkts)) ^ "\n");
	    app (fn(b) => print ((ppBkt b) ^ "\n")) (listBkts h);
	    print "\n\n";
	    print ("find [$5,$6,$7,$8]: " ^ (case find (k1,h) of
						 SOME x => x
					       | NONE => "NONE") ^ "\n");
	    print "\n\n";
	    (let val h' = remove (k2, h)
	     in 
		 print "after remove:";
		 app (fn(b) => print ((ppBkt b) ^ "\n")) (listBkts h') 
	     end);
	    print "\n\n"
	end)
*)

(*   val _ = test ()*)
end


(*
 * $Id: Bloom.sml,v 1.4 2006/07/10 21:51:10 ivan_raikov Exp $
 *
 * 
 * $Log: Bloom.sml,v $
 * Revision 1.4  2006/07/10 21:51:10  ivan_raikov
 * Bug fixes in type tag/hashing routines.
 *
 * Revision 1.3  2006/07/04 21:23:26  ivan_raikov
 * Checkedd in changes related to new phimap.
 *
 * Revision 1.2  2006/07/03 21:01:13  ivan_raikov
 * Repository updated.
 *
 * Revision 1.1.1.1  2006/06/19 19:40:34  ivan_raikov
 * Imported sources
 *
 *
 *)
