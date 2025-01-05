--
--  Copyright (C) 2023-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Numerics;

with CGK.Reals.Elementary_Functions;

with Kinematics;

package body Footpath_Generators.Swing is

   use type CGK.Reals.Real;

   function T_XY_Swing
     (Ratio : CGK.Reals.Real;
      T     : Step_Fase) return CGK.Reals.Real
      with Pre => Ratio in 0.0 .. 1.0;

   function T_Z_Swing (T : Step_Fase) return CGK.Reals.Real;

   ----------------
   -- T_XY_Swing --
   ----------------

   function T_XY_Swing
     (Ratio : CGK.Reals.Real;
      T     : Step_Fase) return CGK.Reals.Real is
   begin
      return
        ((12.0 * T ** 5 - 30.0 * T ** 4 + 20.0 * T ** 3 - 2.0 * T) * Ratio
           + 12.0 * T ** 5 - 30.0 * T ** 4 + 20.0 * T ** 3 - 1.0) / 2.0;
   end T_XY_Swing;

   ---------------
   -- T_Z_Swing --
   ---------------

   function T_Z_Swing (T : Step_Fase) return CGK.Reals.Real is
      use Ada.Numerics;
      use CGK.Reals.Elementary_Functions;

   begin
      if T <= 0.5 then
         return -(Sin (4.0 * Pi * T) - 4.0 * Pi * T) / (2.0 * Pi);

      else
         return (Sin (4.0 * Pi * T) - 4.0 * Pi * T + 4.0 * Pi) / (2.0 * Pi);
      end if;
   end T_Z_Swing;

   ----------
   -- Tick --
   ----------

   overriding procedure Tick (Self : in out Swing_Footpath_Generator) is
      Ratio : constant := 1.0;
      --  ??? Ratio of the swing and stance speed ???

   begin
      --  Update fase in swing state.

      Self.Fase := CGK.Reals.Real'Min (@ + Self.D_Fase, 1.0);

         --  Swing of the leg

      declare
         T_XY    : constant CGK.Reals.Real := T_XY_Swing (Ratio, Self.Fase);
         X       : CGK.Reals.Real;
         Y       : CGK.Reals.Real;
         Z       : CGK.Reals.Real;
         Success : Boolean;

      begin
         X := Self.PEP_X + (Self.AEP_X - Self.PEP_X) * (T_XY + 0.5);
         Y := Self.PEP_Y + (Self.AEP_Y - Self.PEP_Y) * (T_XY + 0.5);
         Z := Self.Base_Z + Self.Height * T_Z_Swing (Self.Fase);

         Kinematics.Set (Self.Leg.Configuration.Position, X, Y, Z);

         Legs.Inverse_Kinematics
           (Self             => Self.Leg.Kinematics_Parameters,
            Desired_Position => Self.Leg.Configuration.Position,
            Found_Posture    => Self.Leg.Configuration.Posture,
            Success          => Success);

         if not Success then
            raise Program_Error;
         end if;
      end;
   end Tick;

end Footpath_Generators.Swing;
