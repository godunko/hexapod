--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with CGK.Reals;

package Footpath_Generators.Swing.Builders
  with Pure
is

   type Swing_Footpath_Generator_Builder is limited private;

   procedure Build
     (Self   : in out Swing_Footpath_Generator_Builder;
      Leg    : not null access Legs.Leg;
      PEP_X  : CGK.Reals.Real;
      PEP_Y  : CGK.Reals.Real;
      AEP_X  : CGK.Reals.Real;
      AEP_Y  : CGK.Reals.Real;
      Base_Z : CGK.Reals.Real;
      Height : CGK.Reals.Real;
      Ticks  : Positive);

   function Value
     (Self : Swing_Footpath_Generator_Builder) return Swing_Footpath_Generator;

private

   type Swing_Footpath_Generator_Builder is limited record
      Valid : Boolean := False;
      Value : Swing_Footpath_Generator;
   end record;

end Footpath_Generators.Swing.Builders;
