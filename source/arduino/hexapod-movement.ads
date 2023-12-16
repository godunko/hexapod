--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Movement of the hexapod.

pragma Restrictions (No_Elaboration_Code);

with Reals;

package Hexapod.Movement is

   Cycle : constant := 3.0;
   --  Length of the cycle.

   Ticks : constant := 100.0;
   --  Number of ticks per second.

   procedure Initialize;

   procedure Prepare;

   procedure Step;

   procedure Set_Step_Length
     (Step_Length_X : Reals.Real;
      Step_Length_Y : Reals.Real);

end Hexapod.Movement;
