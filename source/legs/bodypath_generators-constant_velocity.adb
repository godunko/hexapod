--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with CGK.Primitives.Points_3D;

package body Bodypath_Generators.Constant_Velocity is

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (Self     : Constant_Velocity_Bodypath_Generator'Class;
      Position : in out Kinematics.Position) is
   begin
      CGK.Primitives.Points_3D.Transform (Position, Self.Transformation);
   end Transform;

end Bodypath_Generators.Constant_Velocity;
