--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Footpath_Generators.Stance_Whole_Body.Builders is

   -----------
   -- Build --
   -----------

   procedure Build
     (Self     : in out Stance_Footpath_Generator_Builder;
      Leg      : not null access Legs.Leg;
      Bodypath : not null access
        Bodypath_Generators.Whole_Body.Whole_Body_Bodypath_Generator'Class)
   is
   begin
      Self.Value := (Leg => Leg, Bodypath => Bodypath);
      Self.Valid := True;
   end Build;

   -----------
   -- Value --
   -----------

   function Value
     (Self : Stance_Footpath_Generator_Builder)
      return Stance_Footpath_Generator is
   begin
      if not Self.Valid then
         raise Program_Error;
      end if;

      return Self.Value;
   end Value;

end Footpath_Generators.Stance_Whole_Body.Builders;
