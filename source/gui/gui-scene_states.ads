--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Consolidated robot state information.

pragma Ada_2022;

with CGK.Primitives.Circles_2D;
with CGK.Reals;

with Kinematics;
with Legs;

package GUI.Scene_States is

   type Legs_Posture is array (Legs.Leg_Index) of Kinematics.Posture;

   type Legs_Workspace is
     array (Legs.Leg_Index) of CGK.Primitives.Circles_2D.Circle_2D;

   type Scene_Information is record
      Body_Height     : CGK.Reals.Real := 0.070;

      Legs_Posture    : GUI.Scene_States.Legs_Posture;
      Legs_Workspace  : GUI.Scene_States.Legs_Workspace;

      Ground_Offset_X : CGK.Reals.Real := 0.0;
      Ground_Offset_Y : CGK.Reals.Real := 0.0;
      Ground_Rotate_Z : CGK.Reals.Real := 0.0;
   end record;

end GUI.Scene_States;
