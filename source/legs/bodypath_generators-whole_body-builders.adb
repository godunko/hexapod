--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Bodypath_Generators.Whole_Body.Builders is

   -----------
   -- Build --
   -----------

   procedure Build
     (Self           : in out Whole_Body_Bodypath_Generator_Builder;
      Transformation : CGK.Primitives.Transformations_3D.Transformation_3D;
      Velocity       : Kinematics.Velocity) is
   begin
      Self.Value :=
        (Transformation => Transformation,
         Velocity       => Velocity,
         Configuration  => <>);
      Self.Valid := True;
   end Build;

   ---------------
   -- Generator --
   ---------------

   function Generator
     (Self : Whole_Body_Bodypath_Generator_Builder)
      return Whole_Body_Bodypath_Generator is
   begin
      if not Self.Valid then
         raise Program_Error;
      end if;

      return Self.Value;
   end Generator;

end Bodypath_Generators.Whole_Body.Builders;
