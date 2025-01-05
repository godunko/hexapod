--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Footpath_Generators.Swing.Builders is

   -----------
   -- Build --
   -----------

   procedure Build
     (Self   : in out Swing_Footpath_Generator_Builder;
      Leg    : not null access Legs.Leg;
      PEP_X  : CGK.Reals.Real;
      PEP_Y  : CGK.Reals.Real;
      AEP_X  : CGK.Reals.Real;
      AEP_Y  : CGK.Reals.Real;
      Base_Z : CGK.Reals.Real;
      Height : CGK.Reals.Real;
      Ticks  : Positive)
   is
      use type CGK.Reals.Real;

   begin
      Self.Value :=
        (Leg    => Leg,
         PEP_X  => PEP_X,
         PEP_Y  => PEP_Y,
         AEP_X  => AEP_X,
         AEP_Y  => AEP_Y,
         Base_Z => Base_Z,
         Height => Height,
         Fase   => 0.0,
         D_Fase => 1.0 / CGK.Reals.Real (Ticks));
      Self.Valid := True;
   end Build;

   -----------
   -- Value --
   -----------

   function Value
     (Self : Swing_Footpath_Generator_Builder)
      return Swing_Footpath_Generator is
   begin
      if not Self.Valid then
         raise Program_Error;
      end if;

      return Self.Value;
   end Value;

end Footpath_Generators.Swing.Builders;
