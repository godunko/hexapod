--
--  Copyright (C) 2023-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Root package of the trajectory planner/executor for the Stage 2.
--
--  Gait cycle consists from the few steps. At each step leg do complete
--  swing stage or full/part of stance stage.
--
--  Step planner compute step parameters according to state, and step
--  executor computes coordinates of the legs using fase parameter in range
--  [0 .. 1] at subseqent control ticks.

pragma Restrictions (No_Elaboration_Code);

with Reals;

package Trajectory.Steps is

   pragma Pure;

   use type Reals.Real;

   type Stage_Kind is (Strait, Swing);

   subtype Step_Fase is Reals.Real range 0.0 .. 1.0;

--     subtype Stage_Fase is Reals.Real range 0.0 .. 1.0;
--
--     subtype Stance_Fase is Stage_Fase;
--
--     subtype Swing_Fase is Stage_Fase;

   subtype Trajectory_Position is Reals.Real range -0.5 .. 0.5;

   type Leg_Step_Plan_Descriptor (Stage : Stage_Kind := Strait) is record
      case Stage is
         when Strait =>
            D_X : Reals.Real;
            D_Y : Reals.Real;
            --  Leg's trajectory delta at each control tick.

         when Swing =>
            Length_X       : Reals.Real;
            Length_Y       : Reals.Real;
            --  Projection of the stance trajectory on X and Y axes.

            Start_Position : Trajectory_Position;
            End_Position   : Trajectory_Position;
            --  Start position and end positions of the step on the trajectory.

            Height_Z : Reals.Real;
            --  Height of the swing.
      end case;
   end record;

   type Step_Plan_Descriptor is record
      Ratio : Reals.Real               := 0.0;
      LF    : Leg_Step_Plan_Descriptor := (Strait, 0.0, 0.0);
      LM    : Leg_Step_Plan_Descriptor := (Strait, 0.0, 0.0);
      LH    : Leg_Step_Plan_Descriptor := (Strait, 0.0, 0.0);
      RF    : Leg_Step_Plan_Descriptor := (Strait, 0.0, 0.0);
      RM    : Leg_Step_Plan_Descriptor := (Strait, 0.0, 0.0);
      RH    : Leg_Step_Plan_Descriptor := (Strait, 0.0, 0.0);
   end record;

end Trajectory.Steps;
