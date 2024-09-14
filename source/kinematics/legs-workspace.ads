--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with CGK.Primitives.Circles_2D;

package Legs.Workspace
  with Preelaborate
is

   procedure Compute (Body_Height : Reals.Real);

   procedure Ground_Center
     (Index    : Leg_Index;
      Position : out Kinematics.Position);
   --  Returns center of the workspace at ground level.

   procedure Get_Bounding_Shape
     (Index  : Leg_Index;
      Result : out CGK.Primitives.Circles_2D.Circle_2D);
   --  Returns

   function Get_Bounding_Shape
     (Index  : Leg_Index) return CGK.Primitives.Circles_2D.Circle_2D;

end Legs.Workspace;
