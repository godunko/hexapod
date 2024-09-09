--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Movement of the hexapod.

--  pragma Restrictions (No_Elaboration_Code);

with Reals;

package Hexapod.Movement is

   Cycle : constant := 0.5;
   --  Length of the cycle.

   Ticks : constant := 100;
   --  Number of ticks per second.

   type Gait_Kind is (Stop, Wave, Quadro, Tripod);

   procedure Initialize;

   procedure Configure;

   procedure Step;

   procedure Set_Gait (Gait : Gait_Kind);

   procedure Set_Relative_Velocity
     (V_X : Reals.Real;
      V_Y : Reals.Real);

   procedure Register_Task;
   --  Register task

end Hexapod.Movement;
