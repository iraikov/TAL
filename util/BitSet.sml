(*
 * Non growable dense set in bitvector format.
 * 
 * -- Allen
 *)

structure BitSet :> BITSET =
struct

   structure A = Word8Array
   structure W = Word8
   open A

   infix << >> & ||
   infix sub

   type bitset = array 

   val word  = Word.fromInt 
   val int   = Word.toInt
   val op &  = Word.andb
   val op >> = Word.>>
   val op << = W.<<

   fun error (f,msg) = LibBase.failure{module="BitSet",func=f,msg=msg}

   fun byteOf i = int((word i) >> 0w3)
   fun wmask7 i = W.<< (0w1, ((word i) & 0w7))

   fun create n = array((n+7) div 8, 0wx0)

   fun size a = length(a) * 8

   fun set (a, i) =
   let val byte = byteOf i
       val mask = wmask7 i
   in  update(a, byte, W.orb(a sub byte, mask)) end

   fun fromList (l: bool list) = 
       let
	   val n = List.length l
	   val a = create n

	   fun f (i,x :: l) =
	       (case x of
		    true  => set (a,i) 
		  | false => (); f (i+1,l))
	     | f (i,[]) = ()
       in
	   (f (0,l); a)
       end

   fun reset (a, i) =
   let val byte = byteOf i
       val mask = W.notb(wmask7 i)
   in  update(a, byte, W.andb(a sub byte, mask)) end

   fun clear a = modify (fn _ => 0wx0) a

   fun copy (a) = tabulate (length a, fn i => a sub i)

   fun toString (a) = 
   let fun f i = if i < length a then W.toString(a sub i)::f(i+1) else []
       val s = String.concat(f 0)
   in  "[" ^ s ^ "]" end

   fun contains (a, i) = 
   let val byte = byteOf i
       val mask = wmask7 i
   in  W.andb(A.sub(a, byte), mask) <> 0wx0 end
 
   fun markAndTest (a, i) =
   let val byte = byteOf i
       val mask = wmask7 i
       val word = A.sub(a,byte)
   in  if W.andb(word, mask) <> 0wx0 then
          true
       else 
          (A.update(a, byte, W.orb(word, mask)); false)
   end

   fun unmarkAndTest (a, i) =
   let val byte = byteOf i
       val mask = wmask7 i
       val word = A.sub(a,byte)
   in  if W.andb(word, mask) <> 0wx0 then
          (A.update(a, byte, W.andb(word,W.notb mask)); true)
       else 
          false
   end 

end

