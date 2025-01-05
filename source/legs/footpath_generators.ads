--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Legs;

package Footpath_Generators
  with Pure
is

   --  type Abstract_Footpath_Generator (Leg : not null access Legs.Leg) is
   --    abstract tagged limited private;

   type Abstract_Footpath_Generator is abstract tagged record
      Leg : access Legs.Leg;
   end record;

   not overriding procedure Tick
     (Self : in out Abstract_Footpath_Generator) is abstract;

--  private
--
--     type Abstract_Footpath_Generator (Leg : not null access Legs.Leg) is
--       abstract tagged limited null record;

end Footpath_Generators;
