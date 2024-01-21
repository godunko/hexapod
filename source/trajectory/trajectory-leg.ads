--
--  Copyright (C) 2023-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Generator of the leg trajectory based on oscillator.

with Reals;

package Trajectory.Leg is

   pragma Pure;

   procedure Position_XYZ
     (Base_X   : Reals.Real;
      Base_Y   : Reals.Real;
      Base_Z   : Reals.Real;
      Beta     : Reals.Real;
      Time     : Reals.Real;
      Length_X : Reals.Real;
      Length_Y : Reals.Real;
      Height_Z : Reals.Real;
      X        : out Reals.Real;
      Y        : out Reals.Real;
      Z        : out Reals.Real)
   with Pre => Beta in 0.0 .. 1.0 and Time in 0.0 .. 1.0;

end Trajectory.Leg;
