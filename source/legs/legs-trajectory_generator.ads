--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Generates legs posture for each control loop tick.

--  pragma Restrictions (No_Elaboration_Code);

with CGK.Reals;

with Legs.Trajectory;

package Legs.Trajectory_Generator
  with Preelaborate
is

   Trajectory : access Standard.Legs.Trajectory.Trajectory_Information;

   procedure Initialize;

   procedure Tick;
   --  Computes position of the end-effector and posture of the legs.

   procedure Set_Stance (Leg : Leg_Index);
   --  Sets leg's to use body's trajectory to follow path.

   procedure Set_Swing
     (Leg    : Leg_Index;
      AEP_X  : CGK.Reals.Real;
      AEP_Y  : CGK.Reals.Real;
      Height : CGK.Reals.Real);
   --  Sets leg's trajectory to swing info AEP

end Legs.Trajectory_Generator;
