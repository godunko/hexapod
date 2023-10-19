--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Kinematics;
with Reals;

package Hexapod.Debug is

   pragma Pure;

   function Coordinate_Image (Item : Reals.Real) return String;

   function Position_Image (Item : Kinematics.Position) return String;

end Hexapod.Debug;
