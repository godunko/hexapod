--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Bodypath_Generators.Whole_Body;

package Footpath_Generators.Stance_Whole_Body.Builders
  with Preelaborate
is

   type Stance_Footpath_Generator_Builder is limited private;

   procedure Build
     (Self     : in out Stance_Footpath_Generator_Builder;
      Leg      : not null access Legs.Leg;
      Bodypath : not null access
        Bodypath_Generators.Whole_Body.Whole_Body_Bodypath_Generator'Class);

   function Value
     (Self : Stance_Footpath_Generator_Builder)
      return Stance_Footpath_Generator;

private

   type Stance_Footpath_Generator_Builder is limited record
      Valid : Boolean := False;
      Value : Stance_Footpath_Generator;
   end record;

end Footpath_Generators.Stance_Whole_Body.Builders;
