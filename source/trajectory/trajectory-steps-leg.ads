--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Trajectory.Steps.Leg is

   pragma Pure;

   procedure Position_XYZ
     (Base_X : Reals.Real;
      Base_Y : Reals.Real;
      Base_Z : Reals.Real;
      Plan   : Leg_Step_Plan_Descriptor;
      Ratio  : Reals.Real;
      Fase   : Step_Fase;
      X      : out Reals.Real;
      Y      : out Reals.Real;
      Z      : out Reals.Real);
   --  Compute coordinates of the leg from base base location, step
   --  description and fase of the step oscillator.
   --
   --  @param Ratio Ratio of the swing and stance speed.

end Trajectory.Steps.Leg;