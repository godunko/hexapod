--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Bodypath_Generators
  with Pure
is

   type Abstract_Bodypath_Generator is abstract tagged null record;

   not overriding procedure Tick
     (Self : in out Abstract_Bodypath_Generator) is abstract;

end Bodypath_Generators;
