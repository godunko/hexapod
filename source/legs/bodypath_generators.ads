--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Kinematics;
with Legs;

package Bodypath_Generators
  with Pure
is

   type Abstract_Bodypath_Generator is abstract tagged null record;

   not overriding procedure Tick
     (Self : in out Abstract_Bodypath_Generator) is abstract;

   not overriding procedure Transform
     (Self     : Abstract_Bodypath_Generator;
      Position : in out Kinematics.Position) is abstract;

   not overriding procedure Update
     (Self    : Abstract_Bodypath_Generator;
      Leg     : Legs.Leg_Index;
      Posture : in out Kinematics.Posture) is abstract;

end Bodypath_Generators;
