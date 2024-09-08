--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

package Legs.Workspace
  with Preelaborate
is

   procedure Compute (Body_Height : Reals.Real);

   procedure Ground_Center
     (Index    : Leg_Index;
      Position : out Kinematics.Position);
   --  Returns center of the workspace at ground level.

end Legs.Workspace;
