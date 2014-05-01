
(* 
 * Hash.sml
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *
 *)

structure Hash = 
struct

structure W8A = Word8Array
structure W8V = Word8Vector
structure BA = BitArray


val word = Word.fromInt



(* A function to hash a 32-bit unsigned integer. *)
fun hashWord32 (w) = 
    Word32.mod(Word32.>>(w, 0w7),0w16349: Word32.word)


(* A function to hash a byte.    h = 64 * h + 720 + b  *)
fun hashWord8 (b, h) = 
    let val word = Word.fromInt
	val int  = Word8.toInt
    in
	Word.<<(h, 0w6) + h + 0w720 + (word (int b))
    end
			  
(* fun hashWord8Vector s = Word8Vector.foldl hashWord8 0w0 s *)
fun hashWord8Vector v = 
    (let
	 val word = Word.fromInt
	 val intx = Word.toIntX
	 val len  = Word8Vector.length

	 fun x + y = intx (Word.+ (word x, word y))
		     
	 val sub = Unsafe.Word8Vector.sub
	 fun hash (s, i0: int, e) = 
	     let
		 fun loop (h, i: int) = 
		     if i >= e then h else loop (hashWord8 (sub (s, i), h), i+1)
	     in
		 loop (0w0, i0)
	     end
     in
	 Int.abs (intx (hash (v, 0, len v)))
     end)


fun crc32 (b, h, i0: int) = 
    let
	fun inc x = x+1
	fun dec x = x-1
	fun pos x = x>0
	fun lti x = x<i0

	val sub = BA.sub
		    
	open Word32
	infix << orb xorb andb
	val word32 = fromInt

	val p   = (0wx1EDC6F41: word)
	val p   = (0wx82F63B78: word)
	val p   = (0wx04C11DB7: word)

	val z   = (0w0: word)
	val m   = (0w1: word) << 0w31
		  
	fun loop (h,i) = 
	    let 
		val h  = if (h andb m) = z
			 (* most significant bit of shift reg is set *)
			 then (if sub(b,i) 
			       then ((h << 0w1) orb 0w1) xorb p
			       else (h << 0w1) xorb p)
			 (* shift reg is not divisible by polynomial yet *)
			 else (if sub(b,i) then (h << 0w1) else h)
	    in 
		if (lti i) then loop (h, inc i) else h
	    end
    in
	loop (h, 0)
    end

val fnv8 =
    let
	open Word32
	infix xorb 

	fun word32 w = fromInt (Word8.toInt w)

	val p   = (0wx01000193: word)
	fun ck (h,q) = ((h xorb (word32 q)) * p)
    in 
	(ck, (0wx811C9DC5: word))
    end

val w8rmask = 
    let open Word8
    in
	W8V.fromList ([0wx80: word, 0wx40: word, 0wx20: word, 0wx10: word, 
		       0wx8: word,  0wx4: word,  0wx2: word,  0wx1: word])
    end

fun cksum8 ck (ref b,f,(h0: Word32.word,w0: int,q0: Word8.word ref)) =
    let
	val ++ = Int.+
	val -- = Int.-
	infix ++ -- ==

	fun inc x = x+1
	fun dec x = x-1
	fun pos x = x>0

	fun incw x = Word.+(x,0w1)
	fun decw x = Word.-(x,0w1)
	fun posw x = Word.>(x,0w0)

	val sub = BA.sub
	val len = BA.length

	open Word8
	infix << orb xorb andb

	val tlen = ref 0

	fun loop (b,h: Word32.word,i,j,w: int,q: word ref,f:bool) = 
	    let 
		val _ = if sub(b,j) then (q := (!q orb (W8V.sub(w8rmask,w)))) else ()
							  
		val (h,w) = if (pos w) then  (h,dec w) 
			    else (let 
				      val h = ck(h,!q)
				  in 
				      q := (0w0: word); (h, dec Word8.wordSize)
				  end)
				 
	    in 
		if  (i = j) 
		then (if f then (h,w,q)  else (ck(h,!q),w,q))
		else (loop (b, h, i, inc j, w, q, f))
	    end

	val blen = len b
	val _ = tlen := ((!tlen) ++ blen)
	val (h,w,q) = if (pos blen) 
		      then (loop (b,h0,dec (len b),0,w0,q0,f))
		      else (h0,w0,q0)
    in
	(h,w,q)
    end




end
