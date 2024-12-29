--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Consolidated robot state information.

pragma Ada_2022;

with CGK.Primitives.Circles_2D;
with CGK.Primitives.Points_3D;
with CGK.Reals;

with Kinematics;
with Legs;

package GUI.Scene_States is

   type Leg_Information is record
      Posture    : Kinematics.Posture;
      Joint_1    : CGK.Primitives.Points_3D.Point_3D;
      Joint_2    : CGK.Primitives.Points_3D.Point_3D;
      Joint_3    : CGK.Primitives.Points_3D.Point_3D;
      Effector   : CGK.Primitives.Points_3D.Point_3D;
      --  Posture of the leg, precomputed coordinates of the joints and end
      --  effector.

      Is_Support : Boolean;
      --  Whether leg supports the robot.

      Workspace  : CGK.Primitives.Circles_2D.Circle_2D;
   end record;

   type Leg_Information_Array is array (Legs.Leg_Index) of Leg_Information;

   type Scene_Information is record
      Body_Height     : CGK.Reals.Real := 0.070;

      Legs            : Leg_Information_Array;

      Ground_Offset_X : CGK.Reals.Real := 0.0;
      Ground_Offset_Y : CGK.Reals.Real := 0.0;
      Ground_Rotate_Z : CGK.Reals.Real := 0.0;
   end record;

end GUI.Scene_States;
