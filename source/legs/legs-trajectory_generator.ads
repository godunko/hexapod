--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Generates legs posture for each control loop tick.

--  pragma Restrictions (No_Elaboration_Code);

with CGK.Primitives.Transformations_3D;
with CGK.Reals;

package Legs.Trajectory_Generator
  with Preelaborate
is

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

   procedure Set_Bodypath
     (Transformation : CGK.Primitives.Transformations_3D.Transformation_3D);
   --  Sets transformation

   procedure Accumulated_Transformation
     (X : in out CGK.Reals.Real;
      Y : in out CGK.Reals.Real;
      W : in out CGK.Reals.Real);
   --  Update accumulated transformation for GUI

end Legs.Trajectory_Generator;
