--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;

package body Trajectory.Leg is

   use type Reals.Real;

   function T_XY (Beta : Reals.Real; T_T : Reals.Real) return Reals.Real;

   function T_Z (Beta : Reals.Real; T_T : Reals.Real) return Reals.Real;

   function T_XY_Stance (T : Step_Fase) return Trajectory_Position;

   function T_XY_Swing
     (Ratio : Reals.Real;
      T     : Step_Fase) return Trajectory_Position
      with Pre => Ratio in 0.0 .. 1.0;

   function T_Z_Swing (T : Step_Fase) return Trajectory_Position;

   function Map
     (Value      : Reals.Real;
      From_First : Reals.Real;
      From_Last  : Reals.Real;
      To_First   : Reals.Real;
      To_Last    : Reals.Real) return Reals.Real;

   ---------
   -- Map --
   ---------

   function Map
     (Value      : Reals.Real;
      From_First : Reals.Real;
      From_Last  : Reals.Real;
      To_First   : Reals.Real;
      To_Last    : Reals.Real) return Reals.Real is
   begin
      return
       (Value - From_First) * (To_Last - To_First) / (From_Last - From_First)
          + To_First;
   end Map;

   ------------------
   -- Position_XYZ --
   ------------------

   procedure Position_XYZ
     (Base_X     : Reals.Real;
      Base_Y     : Reals.Real;
      Base_Z     : Reals.Real;
      Descriptor : Leg_Step_Descriptor;
      Ratio      : Reals.Real;
      Fase       : Step_Fase;
      X          : out Reals.Real;
      Y          : out Reals.Real;
      Z          : out Reals.Real)
   is
      T_XY : constant Reals.Real :=
        (case Descriptor.Stage is
           when Stance => T_XY_Stance (Fase),
           when Swing  => T_XY_Swing (Ratio, Fase));
      C_XY : constant Reals.Real :=
        Map (T_XY, -0.5, 0.5, Descriptor.Start_Position, Descriptor.End_Position);
      T_Z  : constant Reals.Real :=
        (case Descriptor.Stage is
           when Stance => 0.0,
           when Swing  => T_Z_Swing (Fase));

   begin
      X := Base_X + Descriptor.Length_X * C_XY;
      Y := Base_Y + Descriptor.Length_Y * C_XY;
      Z := Base_Z + Descriptor.Height_Z * T_Z;
   end Position_XYZ;

   ------------------
   -- Position_XYZ --
   ------------------

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
   is
      T_XY : constant Reals.Real := Trajectory.Leg.T_XY (Beta, Time);
      T_Z  : constant Reals.REal := Trajectory.Leg.T_Z (Beta, Time);

   begin
      X := Base_X + Length_X * T_XY;
      Y := Base_Y + Length_Y * T_XY;
      Z := Base_Z + Height_Z * T_Z;
   end Position_XYZ;

   ----------
   -- T_XY --
   ----------

   function T_XY (Beta : Reals.Real; T_T : Reals.Real) return Reals.Real is
   begin
      if T_T <= Beta then
         return 0.5 - T_T / Beta;

      else
         return
           -(12.0 * T_T ** 5
             + (-30.0 * Beta - 30.0) * T_T ** 4
             + (20.0 * Beta ** 2 + 80.0 * Beta + 20.0) * T_T ** 3
             + (-60.0 * Beta ** 2 - 60.0 * Beta) * T_T ** 2
             + (2.0 * Beta ** 5 - 10.0 * Beta ** 4 + 20.0 * Beta ** 3
               + 40.0 * Beta ** 2 + 10.0 * Beta - 2.0) * T_T
             - Beta ** 6 + 3.0 * Beta ** 5 - 10.0 * Beta ** 3
             - 5.0 * Beta ** 2 + Beta)
           / (2.0 * Beta ** 6 - 10.0 * Beta ** 5 + 20.0 * Beta ** 4
              - 20.0 * Beta ** 3 + 10.0 * Beta ** 2 - 2.0 * Beta);
      end if;
   end T_XY;

   -----------------
   -- T_XY_Stance --
   -----------------

   function T_XY_Stance (T : Swing_Fase) return Trajectory_Position is
   begin
      return 0.5 - T;
   end T_XY_Stance;

   ----------------
   -- T_XY_Swing --
   ----------------

   function T_XY_Swing
     (Ratio : Reals.Real;
      T     : Swing_Fase) return Trajectory_Position is
   begin
      return
        ((12.0 * T ** 5 - 30.0 * T ** 4 + 20.0 * T ** 3 - 2.0 * T) * Ratio
           +12.0 * T ** 5 - 30.0 * T ** 4 + 20.0 * T ** 3 - 1.0) / 2.0;
   end T_XY_Swing;

   ---------
   -- T_Z --
   ---------

   function T_Z (Beta : Reals.Real; T_T : Reals.Real) return Reals.Real is
      use Ada.Numerics;
      use Reals.Elementary_Functions;

   begin
      if T_T <= Beta then
         return 0.0;

      elsif T_T <= (Beta + 1.0) / 2.0 then
         return
           ((Beta - 1.0)
               * Sin ((4.0 * Pi * T_T - 4.0 * Pi * Beta) / (Beta - 1.0))
             - 4.0 * Pi * T_T + 4.0 * Pi * Beta)
           / (2.0 * Pi * Beta - 2.0 * Pi);

      else
         return
           -((Beta - 1.0)
                * Sin ((4.0 * Pi * T_T - 4.0 * Pi * Beta) / (Beta - 1.0))
             - 4.0 * Pi * T_T + 4.0 * Pi)
           / (2.0 * Pi * Beta - 2.0 * Pi);
      end if;
   end T_Z;

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

end Trajectory.Leg;
