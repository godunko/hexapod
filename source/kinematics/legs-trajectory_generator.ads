--
--  Copyright (C) 2024, Vadim Godunko
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

   --  type Leg_Trajectory_Kind is (Linear, Swing);
   --
   --  type Leg_Trajectory_Plan (Kind : Leg_Trajectory_Kind := Linear) is record
   --     case Kind is
   --        when Linear =>
   --           D_X : Reals.Real;
   --           D_Y : Reals.Real;
   --           --  Leg's trajectory delta at each control tick.
   --
   --        when Swing =>
   --           PEP_X    : Reals.Real;
   --           PEP_Y    : Reals.Real;
   --           --  Posterior extreme position: lift-off point
   --           AEP_X    : Reals.Real;
   --           AEP_Y    : Reals.Real;
   --           --  anterior extreme position: touch-down point
   --
   --           Height_Z : Reals.Real;
   --           --  Height of the swing.
   --     end case;
   --  end record;

   --  Position : array (Leg_Index) of Kinematics.Position;
   --  Posture  : array (Leg_Index) of Kinematics.Posture;

   --  procedure Set_Plan (Plan : Leg_Trajectory_Plan);


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
