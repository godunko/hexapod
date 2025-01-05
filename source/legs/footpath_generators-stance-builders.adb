--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Footpath_Generators.Stance.Builders is

   -----------
   -- Build --
   -----------

   procedure Build
     (Self : in out Stance_Footpath_Generator_Builder;
      Leg  : not null access Legs.Leg) is
   begin
      Self.Value := (Leg => Leg);
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

end Footpath_Generators.Stance.Builders;
