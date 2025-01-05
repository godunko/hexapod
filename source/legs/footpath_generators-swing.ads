--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

private with CGK.Reals;

package Footpath_Generators.Swing
  with Pure
is

   type Swing_Footpath_Generator is
     new Abstract_Footpath_Generator with private;

private

   subtype Step_Fase is CGK.Reals.Real range 0.0 .. 1.0;

   type Swing_Footpath_Generator is
     new Abstract_Footpath_Generator with record
      PEP_X  : CGK.Reals.Real;
      PEP_Y  : CGK.Reals.Real;
      --  Posterior extreme position: lift-off point
      AEP_X  : CGK.Reals.Real;
      AEP_Y  : CGK.Reals.Real;
      --  Anterior extreme position: touch-down point

      Base_Z : CGK.Reals.Real;
      Height : CGK.Reals.Real;
      --  Height of the swing.

      Fase   : CGK.Reals.Real;
      D_Fase : Step_Fase;
   end record;

   overriding procedure Tick (Self : in out Swing_Footpath_Generator);

end Footpath_Generators.Swing;
