
(* 
 * Clos.sml
 *
 * System F closure conversion definitions.
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

structure Clos :> INTERFACE =
struct
  open Type
  open Syntax
  open Semant
  open Transform
end
