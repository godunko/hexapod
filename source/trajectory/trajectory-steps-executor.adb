--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Trajectory.Steps.Leg;

package body Trajectory.Steps.Executor is

   procedure Compute
     (Base     : Kinematics.Position;
      Plan     : Leg_Step_Plan_Descriptor;
      Ratio    : Reals.Real;
      Fase     : Reals.Real;
      Position : in out Kinematics.Position);
   --  Compute position of the leg.

   -------------
   -- Compute --
   -------------

   procedure Compute
     (Base     : Kinematics.Position;
      Plan     : Leg_Step_Plan_Descriptor;
      Ratio    : Reals.Real;
      Fase     : Reals.Real;
      Position : in out Kinematics.Position)
   is
      X : Reals.Real := Kinematics.X (Position);
      Y : Reals.Real := Kinematics.Y (Position);
      Z : Reals.Real := Kinematics.Z (Position);

   begin
      Trajectory.Steps.Leg.Position_XYZ
        (Base_Z => Kinematics.Z (Base),
         Plan   => Plan,
         Ratio  => Ratio,
         Fase   => Fase,
         X      => X,
         Y      => Y,
         Z      => Z);

      Kinematics.Set (Position, X, Y, Z);
   end Compute;

   ----------------------
   -- Compute_Position --
   ----------------------

   procedure Compute_Position
     (LF_Base     : Kinematics.Position;
      LM_Base     : Kinematics.Position;
      LH_Base     : Kinematics.Position;
      RF_Base     : Kinematics.Position;
      RM_Base     : Kinematics.Position;
      RH_Base     : Kinematics.Position;
      Plan        : Step_Plan_Descriptor;
      Fase        : Step_Fase;
      LF_Position : in out Kinematics.Position;
      LM_Position : in out Kinematics.Position;
      LH_Position : in out Kinematics.Position;
      RF_Position : in out Kinematics.Position;
      RM_Position : in out Kinematics.Position;
      RH_Position : in out Kinematics.Position) is
   begin
      Compute
        (Base     => LF_Base,
         Plan     => Plan.LF,
         Ratio    => Plan.Ratio,
         Fase     => Fase,
         Position => LF_Position);
      Compute
        (Base     => LM_Base,
         Plan     => Plan.LM,
         Ratio    => Plan.Ratio,
         Fase     => Fase,
         Position => LM_Position);
      Compute
        (Base     => LH_Base,
         Plan     => Plan.LH,
         Ratio    => Plan.Ratio,
         Fase     => Fase,
         Position => LH_Position);
      Compute
        (Base     => RF_Base,
         Plan     => Plan.RF,
         Ratio    => Plan.Ratio,
         Fase     => Fase,
         Position => RF_Position);
      Compute
        (Base     => RM_Base,
         Plan     => Plan.RM,
         Ratio    => Plan.Ratio,
         Fase     => Fase,
         Position => RM_Position);
      Compute
        (Base     => RH_Base,
         Plan     => Plan.RH,
         Ratio    => Plan.Ratio,
         Fase     => Fase,
         Position => RH_Position);
   end Compute_Position;

end Trajectory.Steps.Executor;

