--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Footpath_Generators.Stance_Whole_Body is

   ----------
   -- Tick --
   ----------

   overriding procedure Tick (Self : in out Stance_Footpath_Generator) is
   begin
      Self.Bodypath.Update
        (Self.Leg.Index, Self.Leg.Configuration.Posture);
      Self.Leg.Configuration.Position :=
        Legs.Forward_Kinematics
          (Self.Leg.all,
           Self.Leg.Configuration.Posture);
   end Tick;

end Footpath_Generators.Stance_Whole_Body;
