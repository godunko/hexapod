--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Trajectory.Leg;

package body Trajectory.Gait is

   procedure Compute
     (Base     : Kinematics.Position;
      Beta     : Reals.Real;
      Fase     : Reals.Real;
      Cycle    : Reals.Real;
      Time     : Reals.Real;
      Length_X : Reals.Real;
      Length_Y : Reals.Real;
      Height_Z : Reals.Real;
      Position : out Kinematics.Position);

   -------------
   -- Compute --
   -------------

   procedure Compute
     (Base     : Kinematics.Position;
      Beta     : Reals.Real;
      Fase     : Reals.Real;
      Cycle    : Reals.Real;
      Time     : Reals.Real;
      Length_X : Reals.Real;
      Length_Y : Reals.Real;
      Height_Z : Reals.Real;
      Position : out Kinematics.Position)
   is
      use type Reals.Real;

      T : Reals.Real := Time / Cycle - Fase;
      X : Reals.Real;
      Y : Reals.Real;
      Z : Reals.Real;

   begin
      if T < 0.0 then
         T := @ + 1.0;

      elsif T > 1.0 then
         T := @ - 1.0;
      end if;

      Trajectory.Leg.Position_XYZ
        (Base_X   => Kinematics.X (Base),
         Base_Y   => Kinematics.Y (Base),
         Base_Z   => Kinematics.Z (Base),
         Beta     => Beta,
         Time     => T,
         Length_X => Length_X,
         Length_Y => Length_Y,
         Height_Z => Height_Z,
         X        => X,
         Y        => Y,
         Z        => Z);

      Kinematics.Set (Position, X, Y, Z);
   end Compute;

   --------------
   -- Position --
   --------------

   procedure Position
     (Descriptor : Gait_Descriptor;
      LF_Base    : Kinematics.Position;
      LM_Base    : Kinematics.Position;
      LH_Base    : Kinematics.Position;
      RF_Base    : Kinematics.Position;
      RM_Base    : Kinematics.Position;
      RH_Base    : Kinematics.Position;
      Cycle      : Reals.Real;
      Time       : Reals.Real;
      Length_X   : Reals.Real;
      Length_Y   : Reals.Real;
      Height_Z   : Reals.Real;
      LF_Position : out Kinematics.Position;
      LM_Position : out Kinematics.Position;
      LH_Position : out Kinematics.Position;
      RF_Position : out Kinematics.Position;
      RM_Position : out Kinematics.Position;
      RH_Position : out Kinematics.Position) is
   begin
      Compute
        (Base     => LF_Base,
         Beta     => Descriptor.Duty_Factor,
         Fase     => Descriptor.LF_Fase,
         Cycle    => Cycle,
         Time     => Time,
         Length_X => Length_X,
         Length_Y => Length_Y,
         Height_Z => Height_Z,
         Position => LF_Position);
      Compute
        (Base     => LM_Base,
         Beta     => Descriptor.Duty_Factor,
         Fase     => Descriptor.LM_Fase,
         Cycle    => Cycle,
         Time     => Time,
         Length_X => Length_X,
         Length_Y => Length_Y,
         Height_Z => Height_Z,
         Position => LM_Position);
      Compute
        (Base     => LH_Base,
         Beta     => Descriptor.Duty_Factor,
         Fase     => Descriptor.LH_Fase,
         Cycle    => Cycle,
         Time     => Time,
         Length_X => Length_X,
         Length_Y => Length_Y,
         Height_Z => Height_Z,
         Position => LH_Position);
      Compute
        (Base     => RF_Base,
         Beta     => Descriptor.Duty_Factor,
         Fase     => Descriptor.RF_Fase,
         Cycle    => Cycle,
         Time     => Time,
         Length_X => Length_X,
         Length_Y => Length_Y,
         Height_Z => Height_Z,
         Position => RF_Position);
      Compute
        (Base     => RM_Base,
         Beta     => Descriptor.Duty_Factor,
         Fase     => Descriptor.RM_Fase,
         Cycle    => Cycle,
         Time     => Time,
         Length_X => Length_X,
         Length_Y => Length_Y,
         Height_Z => Height_Z,
         Position => RM_Position);
      Compute
        (Base     => RH_Base,
         Beta     => Descriptor.Duty_Factor,
         Fase     => Descriptor.RH_Fase,
         Cycle    => Cycle,
         Time     => Time,
         Length_X => Length_X,
         Length_Y => Length_Y,
         Height_Z => Height_Z,
         Position => RH_Position);
   end Position;

end Trajectory.Gait;
