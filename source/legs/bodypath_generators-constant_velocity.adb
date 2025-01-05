--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with CGK.Primitives.Points_3D;

package body Bodypath_Generators.Constant_Velocity is

   ---------------
   -- Transform --
   ---------------

   overriding procedure Transform
     (Self     : Constant_Velocity_Bodypath_Generator;
      Position : in out Kinematics.Position) is
   begin
      CGK.Primitives.Points_3D.Transform (Position, Self.Transformation);
   end Transform;

end Bodypath_Generators.Constant_Velocity;
