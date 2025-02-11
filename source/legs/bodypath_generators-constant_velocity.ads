--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

private with CGK.Primitives.Transformations_3D;

with Kinematics;

package Bodypath_Generators.Constant_Velocity
  with Preelaborate
is

   type Constant_Velocity_Bodypath_Generator is
     new Abstract_Bodypath_Generator with private
       with Preelaborable_Initialization;

   overriding procedure Tick
     (Self : in out Constant_Velocity_Bodypath_Generator) is null;

   procedure Transform
     (Self     : Constant_Velocity_Bodypath_Generator'Class;
      Position : in out Kinematics.Position);

private

   type Constant_Velocity_Bodypath_Generator is
     new Abstract_Bodypath_Generator with record
      Transformation : CGK.Primitives.Transformations_3D.Transformation_3D;
      Velocity       : Kinematics.Velocity;
   end record;

end Bodypath_Generators.Constant_Velocity;
