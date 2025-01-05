--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Footpath_Generators.Stance
  with Preelaborate
is

   type Stance_Footpath_Generator is
     new Abstract_Footpath_Generator with private;

private

   type Stance_Footpath_Generator is
     new Abstract_Footpath_Generator with record
      null;
   end record;

   overriding procedure Tick (Self : in out Stance_Footpath_Generator);

end Footpath_Generators.Stance;
