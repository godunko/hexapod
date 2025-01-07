--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Bodypath_Generators.Constant_Velocity.Builders is

   -----------
   -- Build --
   -----------

   procedure Build
     (Self           : in out Constant_Velocity_Bodypath_Generator_Builder;
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
     (Self : Constant_Velocity_Bodypath_Generator_Builder)
      return Constant_Velocity_Bodypath_Generator is
   begin
      if not Self.Valid then
         raise Program_Error;
      end if;

      return Self.Value;
   end Generator;

end Bodypath_Generators.Constant_Velocity.Builders;
