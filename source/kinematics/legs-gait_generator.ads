--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Generates legs gait on control loop tick.

--  pragma Restrictions (No_Elaboration_Code);

with CGK.Reals;

package Legs.Gait_Generator
  --  with Preelaborate
is

   procedure Initialize;

   procedure Tick;
   --  Computes gait and updates legs' trajectory when necessary.

   procedure Set_Velocity
     (RVX : CGK.Reals.Real;
      RVY : CGK.Reals.Real);
   --  Sets desired velocity

end Legs.Gait_Generator;
