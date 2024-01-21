--
--  Copyright (C) 2023-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Generator of the leg trajectory.
--
--  There are two stages of the movement of the leg: stance and swing.
--  Movement is divided onto steps. In each step leg can do full swing
--  stage or full/part of stance stage. Step fase is number in [0 .. 1]
--  range. To compensate speed of leg relative to the body additional
--  parameter is provided. Its value is ratio of the swing stage to stance
--  stage.

with Reals;

package Trajectory.Leg is

   pragma Pure;

   use type Reals.Real;

   type Stage_Kind is (Stance, Swing);

   subtype Stage_Fase is Reals.Real range 0.0 .. 1.0;

   subtype Step_Fase is Reals.Real range 0.0 .. 1.0;

   subtype Stance_Fase is Stage_Fase;

   subtype Swing_Fase is Stage_Fase;

   subtype Trajectory_Position is Reals.Real range -0.5 .. 0.5;

   type Leg_Step_Descriptor (Stage : Stage_Kind := Stance) is record
      Length_X       : Reals.Real;
      Length_Y       : Reals.Real;
      --  Projection of the stance trajectory on X and Y axes.

      Start_Position : Trajectory_Position;
      End_Position   : Trajectory_Position;
      --  Start position and end positions of the step on the trajectory.

      case Stage is
         when Stance =>
            null;

         when Swing =>
            Height_Z : Reals.Real;
            --  Height of the swing.
      end case;
   end record;

   procedure Position_XYZ
     (Base_X     : Reals.Real;
      Base_Y     : Reals.Real;
      Base_Z     : Reals.Real;
      Descriptor : Leg_Step_Descriptor;
      Ratio      : Reals.Real;
      Fase       : Step_Fase;
      X          : out Reals.Real;
      Y          : out Reals.Real;
      Z          : out Reals.Real);
   --  Compute coordinates of the leg from base base location, step
   --  description and fase of the step oscillator.
   --
   --  @param Ratio Ratio of the swing and stance speed.

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
