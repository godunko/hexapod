--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Reals.Utilities is

   ---------
   -- Map --
   ---------

   function Map
     (Value      : Reals.Real;
      From_First : Reals.Real;
      From_Last  : Reals.Real;
      To_First   : Reals.Real;
      To_Last    : Reals.Real) return Reals.Real is
   begin
      return
        (Value - From_First) * (To_Last - To_First) / (From_Last - From_First)
           + To_First;
   end Map;

end Reals.Utilities;