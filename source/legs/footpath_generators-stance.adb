--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with CGK.Primitives.Points_2D;
with CGK.Primitives.Points_3D;
with CGK.Reals;

with Kinematics;
with Legs.Trajectory;
with Legs.Trajectory_Generator;

package body Footpath_Generators.Stance is

   ----------
   -- Tick --
   ----------

   overriding procedure Tick (Self : in out Stance_Footpath_Generator) is
      X       : CGK.Reals.Real;
      Y       : CGK.Reals.Real;
      Z       : CGK.Reals.Real;
      Point   : CGK.Primitives.Points_2D.Point_2D;
      Success : Boolean;

   begin
      X := CGK.Primitives.Points_3D.X (Self.Leg.Configuration.Position);
      Y := CGK.Primitives.Points_3D.Y (Self.Leg.Configuration.Position);
      Z := CGK.Primitives.Points_3D.Z (Self.Leg.Configuration.Position);

      Point := CGK.Primitives.Points_2D.Create_Point_2D (X, Y);
      --  Legs.Trajectory.Transform (Self.Trajectory.all, Point);
      Legs.Trajectory.Transform
        (Legs.Trajectory_Generator.Trajectory.all, Point);
      X := CGK.Primitives.Points_2D.X (Point);
      Y := CGK.Primitives.Points_2D.Y (Point);

      Kinematics.Set (Self.Leg.Configuration.Position, X, Y, Z);

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
