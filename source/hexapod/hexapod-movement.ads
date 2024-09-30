--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Movement of the hexapod.

--  pragma Restrictions (No_Elaboration_Code);

with CGK.Reals;

package Hexapod.Movement is

   Cycle : constant := 0.5;
   --  Length of the cycle.

   Ticks : constant := 100;
   --  Number of ticks per second.

   procedure Initialize;

   procedure Configure;

   procedure Step;

   procedure Set_Relative_Velocity
     (V_X : CGK.Reals.Real;
      V_Y : CGK.Reals.Real;
      V_W : CGK.Reals.Real);

   procedure Register_Task;
   --  Register task

end Hexapod.Movement;
