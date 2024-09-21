--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

--  with CGK.Primitives.Analytical_Intersections_2D;
--  with CGK.Primitives.Circles_2D;
--  with CGK.Primitives.Directions_2D.Builders;
--  with CGK.Primitives.Lines_2D;
--  with CGK.Primitives.Points_2D;
--  with CGK.Primitives.Points_2D.Containers;
with CGK.Reals;

with Legs.Trajectory;
with Legs.Workspace;

procedure Trajectory_Path_Circle is

   use CGK.Reals;

   --  Angular_Tolerance : constant := Ada.Numerics.Pi / 256.0;
   --
   --  use type CGK.Reals.Real;
   --  use type CGK.Primitives.Directions_2D.Direction_2D;
   --  use type CGK.Primitives.Points_2D.Containers.Point_2D_Array_Offset;
   --
   --  Direction_Builder :
   --    CGK.Primitives.Directions_2D.Builders.Direction_2D_Builder;
   --
   --  Boundary      : CGK.Primitives.Circles_2D.Circle_2D;
   --  Restricted    : CGK.Primitives.Circles_2D.Circle_2D;
   --  Direction     : CGK.Primitives.Directions_2D.Direction_2D;
   --  Path          : CGK.Primitives.Lines_2D.Line_2D;
   --  Intersections :
   --    CGK.Primitives.Analytical_Intersections_2D.Analytical_Intersection_2D;

begin
   --  Ada.Text_IO.Put_Line ("'Epsilon       " & CGK.Reals.Real'Image (CGK.Reals.Real'Epsilon));
   --  Ada.Text_IO.Put_Line ("'Model_Epsilon " & CGK.Reals.Real'Image (CGK.Reals.Real'Model_Epsilon));
   --  Ada.Text_IO.Put_Line ("'Model_Small   " & CGK.Reals.Real'Image (CGK.Reals.Real'Model_Small));

   Legs.Initialize;
   Legs.Workspace.Compute (0.030);

   Legs.Trajectory.Print_Workspace;

   Legs.Trajectory.Center ((0.5, 0.0, 1.0));
   --  Legs.Trajectory.Center ((0.0, 0.0, 1.0));

   New_Line;

   --  Legs.Trajectory.Center ((-0.5, 0.0, 1.0));
   --
   --  New_Line;
   --  New_Line;
   --
   --  Legs.Trajectory.Center ((0.5, 0.0, -1.0));
   --  --  Legs.Workspace.Get_Bounding_Shape
   --  --    (Legs.Left_Front, Boundary);
   --
   --  New_Line;
   --
   --  Legs.Trajectory.Center ((-0.5, 0.0, -1.0));
end Trajectory_Path_Circle;
