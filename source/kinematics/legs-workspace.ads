--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

--  with Kinematics;
--  with Reals;
--
package Legs.Workspace
  with Preelaborate
is

   --  type Workspace is record
   --     Center_X : Reals.Real;
   --     Center_Y : Reals.Real;
   --     Center_Z : Reals.Real;
   --     Radius   : Reals.Real;
   --  end record;
   --  --  Leg's workspace at ground level.

   procedure Compute (Body_Height : Reals.Real);

   procedure Ground_Center
     (Index    : Leg_Index;
      Position : out Kinematics.Position);
   --  Returns center of the workspace at ground level.

end Legs.Workspace;
