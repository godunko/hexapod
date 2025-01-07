--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

private with CGK.Primitives.Transformations_3D;

private with Kinematics.Jacobians.Whole_Body;

package Bodypath_Generators.Constant_Velocity
  with Preelaborate
is

   type Constant_Velocity_Bodypath_Generator is
     new Abstract_Bodypath_Generator with private
       with Preelaborable_Initialization;

   overriding procedure Tick
     (Self : in out Constant_Velocity_Bodypath_Generator);

   overriding procedure Transform
     (Self     : Constant_Velocity_Bodypath_Generator;
      Position : in out Kinematics.Position);

   overriding procedure Update
     (Self    : Constant_Velocity_Bodypath_Generator;
      Leg     : Legs.Leg_Index;
      Posture : in out Kinematics.Posture);

private

   type Constant_Velocity_Bodypath_Generator is
     new Abstract_Bodypath_Generator with record
      Transformation : CGK.Primitives.Transformations_3D.Transformation_3D;
      Velocity       : Kinematics.Velocity;
      Configuration  : Kinematics.Jacobians.Whole_Body.Vector_18;
   end record;

end Bodypath_Generators.Constant_Velocity;
