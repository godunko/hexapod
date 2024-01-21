--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Reals.Utilities is

   pragma Pure;

   function Map
     (Value      : Reals.Real;
      From_First : Reals.Real;
      From_Last  : Reals.Real;
      To_First   : Reals.Real;
      To_Last    : Reals.Real) return Reals.Real;

end Reals.Utilities;