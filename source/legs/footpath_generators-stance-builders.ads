--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Footpath_Generators.Stance.Builders
  with Preelaborate
is

   type Stance_Footpath_Generator_Builder is limited private;

   procedure Build
     (Self : in out Stance_Footpath_Generator_Builder;
      Leg  : not null access Legs.Leg);

   function Value
     (Self : Stance_Footpath_Generator_Builder)
      return Stance_Footpath_Generator;

private

   type Stance_Footpath_Generator_Builder is limited record
      Valid : Boolean := False;
      Value : Stance_Footpath_Generator;
   end record;

end Footpath_Generators.Stance.Builders;
