--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Footpath_Generators.Stance is

   ----------
   -- Tick --
   ----------

   overriding procedure Tick (Self : in out Stance_Footpath_Generator) is
      Success : Boolean;

   begin
      Self.Bodypath.Transform (Self.Leg.Configuration.Position);

      Legs.Inverse_Kinematics
        (Self             => Self.Leg.Kinematics_Parameters,
         Desired_Position => Self.Leg.Configuration.Position,
         Found_Posture    => Self.Leg.Configuration.Posture,
         Success          => Success);

      if not Success then
         raise Program_Error;
      end if;
   end Tick;

end Footpath_Generators.Stance;
