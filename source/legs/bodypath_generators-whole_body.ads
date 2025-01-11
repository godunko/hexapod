--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

private with CGK.Primitives.Transformations_3D;

with Kinematics;
private with Kinematics.Jacobians.Whole_Body;
with Legs;

package Bodypath_Generators.Whole_Body
  with Preelaborate
is

   type Whole_Body_Bodypath_Generator is
     new Abstract_Bodypath_Generator with private
       with Preelaborable_Initialization;

   overriding procedure Tick
     (Self : in out Whole_Body_Bodypath_Generator);

   procedure Transform
     (Self     : Whole_Body_Bodypath_Generator'Class;
      Position : in out Kinematics.Position);

   procedure Update
     (Self    : Whole_Body_Bodypath_Generator'Class;
      Leg     : Legs.Leg_Index;
      Posture : in out Kinematics.Posture);

private

   type Whole_Body_Bodypath_Generator is
     new Abstract_Bodypath_Generator with record
      Transformation : CGK.Primitives.Transformations_3D.Transformation_3D;
      Velocity       : Kinematics.Velocity;
      Configuration  : Kinematics.Jacobians.Whole_Body.Vector_18;
   end record;

end Bodypath_Generators.Whole_Body;
