--
--  Copyright (C) 2023-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;

package body Trajectory.Leg is

   use type Reals.Real;

   function T_XY (Beta : Reals.Real; T_T : Reals.Real) return Reals.Real;

   function T_Z (Beta : Reals.Real; T_T : Reals.Real) return Reals.Real;

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

end Trajectory.Leg;
