--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Trajectory.Steps.Leg is

   pragma Pure;

   procedure Position_XYZ
     (Base_Z : Reals.Real;
      Plan   : Leg_Step_Plan_Descriptor;
      Ratio  : Reals.Real;
      Fase   : Step_Fase;
      X      : in out Reals.Real;
      Y      : in out Reals.Real;
      Z      : in out Reals.Real);
   --  Compute coordinates of the leg from base base location, step
   --  description and fase of the step oscillator.
   --
   --  @param Ratio Ratio of the swing and stance speed.

end Trajectory.Steps.Leg;
