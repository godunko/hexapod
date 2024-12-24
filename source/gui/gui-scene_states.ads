--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Consolidated robot state information.

pragma Ada_2022;

with CGK.Reals;

with Kinematics;
with Legs;

package GUI.Scene_States is

   type Legs_Posture is array (Legs.Leg_Index) of Kinematics.Posture;

   type Scene_Information is record
      Legs_Posture    : GUI.Scene_States.Legs_Posture;

      Ground_Offset_X : CGK.Reals.Real := 0.0;
      Ground_Offset_Y : CGK.Reals.Real := 0.0;
      Ground_Rotate_Z : CGK.Reals.Real := 0.0;
   end record;

end GUI.Scene_States;
