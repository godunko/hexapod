--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;

with Reals.Utilities;

package body Trajectory.Steps.Leg is

   function T_XY_Stance (T : Step_Fase) return Trajectory_Position;

   function T_XY_Swing
     (Ratio : Reals.Real;
      T     : Step_Fase) return Trajectory_Position
      with Pre => Ratio in 0.0 .. 1.0;

   function T_Z_Swing (T : Step_Fase) return Trajectory_Position;

   ------------------
   -- Position_XYZ --
   ------------------

   procedure Position_XYZ
     (Base_X : Reals.Real;
      Base_Y : Reals.Real;
      Base_Z : Reals.Real;
      Plan   : Leg_Step_Plan_Descriptor;
      Ratio  : Reals.Real;
      Fase   : Step_Fase;
      X      : out Reals.Real;
      Y      : out Reals.Real;
      Z      : out Reals.Real)
   is
      T_XY : constant Reals.Real :=
        (case Plan.Stage is
           when Stance => T_XY_Stance (Fase),
           when Swing  => T_XY_Swing (Ratio, Fase));
      C_XY : constant Reals.Real :=
        Reals.Utilities.Map
         (T_XY, -0.5, 0.5, Plan.Start_Position, Plan.End_Position);
      T_Z  : constant Reals.Real :=
        (case Plan.Stage is
           when Stance => 0.0,
           when Swing  => T_Z_Swing (Fase));

   begin
      X := Base_X + Plan.Length_X * C_XY;
      Y := Base_Y + Plan.Length_Y * C_XY;
      Z :=
        Base_Z
          + (case Plan.Stage is
               when Stance => 0.0,
               when Swing  => Plan.Height_Z * T_Z_Swing (Fase));
   end Position_XYZ;

   -----------------
   -- T_XY_Stance --
   -----------------

   function T_XY_Stance (T : Step_Fase) return Trajectory_Position is
   begin
      return 0.5 - T;
   end T_XY_Stance;

   ----------------
   -- T_XY_Swing --
   ----------------

   function T_XY_Swing
     (Ratio : Reals.Real;
      T     : Step_Fase) return Trajectory_Position is
   begin
      return
        ((12.0 * T ** 5 - 30.0 * T ** 4 + 20.0 * T ** 3 - 2.0 * T) * Ratio
           +12.0 * T ** 5 - 30.0 * T ** 4 + 20.0 * T ** 3 - 1.0) / 2.0;
   end T_XY_Swing;

   ---------------
   -- T_Z_Swing --
   ---------------

   function T_Z_Swing (T : Step_Fase) return Trajectory_Position is
      use Ada.Numerics;
      use Reals.Elementary_Functions;

   begin
      if T <= 0.5 then
         return -(Sin(4.0 * Pi * T) - 4.0 * Pi * T) / (2.0 * Pi);

      else
         return (Sin(4.0 * Pi * T) - 4.0 * Pi * T + 4.0 * Pi) / (2.0 * Pi);
      end if;
   end T_Z_Swing;

end Trajectory.Steps.Leg;