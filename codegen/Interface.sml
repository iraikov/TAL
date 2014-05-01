
(* 
 * Interface.sml
 *
 * Code generation interface definitions.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

signature MAIN =
sig
    include EVAL

    val evalFromString   : string -> (value * typ)
    val evalFromFile     : string -> (value * typ)
    val interactiveEval  : unit -> unit
    val run              : string * string list -> unit
end

signature INTERFACE =
sig
    include MAIN
end
