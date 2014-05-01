
(* 
 * SF.sml
 *
 * System F definitions
 *
 * Copyright 2005 Ivan Raikov and the Georgia Institute of Technology.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 *)

structure SF: INTERFACE =
struct
  open Syntax
  open Type
  open Elab
  open Parser
  open PrettyPrint
end
