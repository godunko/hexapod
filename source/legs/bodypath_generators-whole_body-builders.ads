--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with CGK.Primitives.Transformations_3D;

package Bodypath_Generators.Whole_Body.Builders
  with Preelaborate
is

   type Whole_Body_Bodypath_Generator_Builder is limited private
     with Preelaborable_Initialization;

   procedure Build
     (Self           : in out Whole_Body_Bodypath_Generator_Builder;
      Transformation : CGK.Primitives.Transformations_3D.Transformation_3D;
      Velocity       : Kinematics.Velocity);

   function Generator
     (Self : Whole_Body_Bodypath_Generator_Builder)
      return Whole_Body_Bodypath_Generator;

private

   type Whole_Body_Bodypath_Generator_Builder is limited record
      Valid : Boolean := False;
      Value : Whole_Body_Bodypath_Generator;
   end record;

end Bodypath_Generators.Whole_Body.Builders;
