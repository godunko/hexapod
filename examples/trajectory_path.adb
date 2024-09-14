--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;
with Ada.Text_IO;

with CGK.Primitives.Analytical_Intersections_2D;
with CGK.Primitives.Circles_2D;
with CGK.Primitives.Directions_2D.Builders;
with CGK.Primitives.Lines_2D;
with CGK.Primitives.Points_2D;
with CGK.Primitives.Points_2D.Containers;
with CGK.Reals;

with Legs.Workspace;

procedure Trajectory_Path is

   Angular_Tolerance : constant := Ada.Numerics.Pi / 256.0;

   use type CGK.Reals.Real;
   use type CGK.Primitives.Directions_2D.Direction_2D;
   use type CGK.Primitives.Points_2D.Containers.Point_2D_Array_Offset;

   Direction_Builder :
     CGK.Primitives.Directions_2D.Builders.Direction_2D_Builder;

   Boundary      : CGK.Primitives.Circles_2D.Circle_2D;
   Restricted    : CGK.Primitives.Circles_2D.Circle_2D;
   Direction     : CGK.Primitives.Directions_2D.Direction_2D;
   Path          : CGK.Primitives.Lines_2D.Line_2D;
   Intersections :
     CGK.Primitives.Analytical_Intersections_2D.Analytical_Intersection_2D;

begin
   Ada.Text_IO.Put_Line ("'Epsilon       " & CGK.Reals.Real'Image (CGK.Reals.Real'Epsilon));
   Ada.Text_IO.Put_Line ("'Model_Epsilon " & CGK.Reals.Real'Image (CGK.Reals.Real'Model_Epsilon));
   Ada.Text_IO.Put_Line ("'Model_Small   " & CGK.Reals.Real'Image (CGK.Reals.Real'Model_Small));

   Legs.Initialize;
   Legs.Workspace.Compute (0.030);

   Legs.Workspace.Get_Bounding_Shape
     (Legs.Left_Front, Boundary);

   Restricted :=
     CGK.Primitives.Circles_2D.Create_Circle_2D
       (Center => CGK.Primitives.Circles_2D.Center (Boundary),
        Radius => CGK.Primitives.Circles_2D.Radius (Boundary) / 2.0);

   Direction := CGK.Primitives.Directions_2D.Create_Direction_2D (1.0, 1.0);

   Path :=
     CGK.Primitives.Lines_2D.Create_Line_2D
       (Point     => CGK.Primitives.Circles_2D.Center (Restricted),
        Direction => Direction);

   CGK.Primitives.Analytical_Intersections_2D.Intersect
     (Intersections, Path, Restricted);

   if CGK.Primitives.Analytical_Intersections_2D.Length (Intersections) <= 1
   then
      raise Program_Error;
   end if;

   CGK.Primitives.Directions_2D.Builders.Build
     (Self => Direction_Builder,
      From => CGK.Primitives.Circles_2D.Center (Restricted),
      To   =>
        CGK.Primitives.Analytical_Intersections_2D.Point (Intersections, 1));

   if CGK.Primitives.Directions_2D.Is_Equal
        (Direction,
         CGK.Primitives.Directions_2D.Builders.Direction (Direction_Builder),
         Angular_Tolerance)
   then
      raise Program_Error;

   else
      CGK.Primitives.Directions_2D.Builders.Build
        (Self => Direction_Builder,
         From => CGK.Primitives.Circles_2D.Center (Restricted),
         To   =>
           CGK.Primitives.Analytical_Intersections_2D.Point
             (Intersections, 2));

      if CGK.Primitives.Directions_2D.Is_Equal
           (Direction,
            CGK.Primitives.Directions_2D.Builders.Direction (Direction_Builder),
            Angular_Tolerance)
      then
         raise Program_Error;

      else
         raise Program_Error;
      end if;
   end if;
end Trajectory_Path;
