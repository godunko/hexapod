--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with CGK.Primitives.Transformations_3D;

package Bodypath_Generators.Constant_Velocity.Builders
  with Pure
is

   type Constant_Velocity_Bodypath_Generator_Builder is limited private
     with Preelaborable_Initialization;

   procedure Build
     (Self           : in out Constant_Velocity_Bodypath_Generator_Builder;
      Transformation : CGK.Primitives.Transformations_3D.Transformation_3D);

   function Generator
     (Self : Constant_Velocity_Bodypath_Generator_Builder)
      return Constant_Velocity_Bodypath_Generator;

private

   type Constant_Velocity_Bodypath_Generator_Builder is limited record
      Valid : Boolean := False;
      Value : Constant_Velocity_Bodypath_Generator;
   end record;

end Bodypath_Generators.Constant_Velocity.Builders;
