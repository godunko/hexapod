--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Bodypath_Generators.Whole_Body;

package Footpath_Generators.Stance_Whole_Body
  with Preelaborate
is

   type Stance_Footpath_Generator is
     new Abstract_Footpath_Generator with private;

private

   type Stance_Footpath_Generator is
     new Abstract_Footpath_Generator with record
      Bodypath : access
        Bodypath_Generators.Whole_Body.Whole_Body_Bodypath_Generator'Class;
   end record;

   overriding procedure Tick (Self : in out Stance_Footpath_Generator);

end Footpath_Generators.Stance_Whole_Body;
