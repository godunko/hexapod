--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Kinematics;
with Reals;

package Hexapod.Debug is

   pragma Pure;

   function Coordinate_Image (Item : Reals.Real) return String;

   function Position_Image (Item : Kinematics.Position) return String;

   function Angle_Image (Item : Reals.Real) return String;

   function Posture_Image (Item : Kinematics.Posture) return String;

   function Parametric_Image
     (Item      : Reals.Real;
      Precision : Positive := 3) return String;
   --  Formats number in the -1.0 .. 1.0 range with given number of digits
   --  after decimal point.

end Hexapod.Debug;
