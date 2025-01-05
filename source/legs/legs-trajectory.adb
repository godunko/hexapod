--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;
--  pragma Restrictions (No_Elaboration_Code);

--  with Ada.Numerics;

with CGK.Mathematics.Vectors_3;
with CGK.Primitives.Analytical_Intersections_2D;
with CGK.Primitives.Directions_2D.Builders;
with CGK.Primitives.Lines_2D;
with CGK.Primitives.Points_2D.Containers;
with CGK.Primitives.Vectors_2D;
with CGK.Primitives.XYs;
with CGK.Primitives.XYZs;
with CGK.Reals.Elementary_Functions;

with Debug.Log;
with Hexapod.Parameters.Control_Cycle;
with Legs.Workspace;

package body Legs.Trajectory is

   use CGK.Primitives.Analytical_Intersections_2D;
   use CGK.Primitives.Circles_2D;
   use CGK.Primitives.Directions_2D.Builders;
   use CGK.Primitives.Lines_2D;
   use CGK.Primitives.Points_2D;
   use CGK.Primitives.Transformations_2D;
   use CGK.Primitives.Vectors_2D;

   use type CGK.Primitives.Points_2D.Containers.Point_2D_Array_Count;

   function Is_Forward
     (Path  : CGK.Primitives.Lines_2D.Line_2D;
      Point : CGK.Primitives.Points_2D.Point_2D) return Boolean;

   --  package Real_IO is new Ada.Text_IO.Float_IO (CGK.Reals.Real);

   --  procedure Put_Length_Image (Item : CGK.Reals.Real);
   --
   --  procedure Put_Angle_Image (Item : CGK.Reals.Real);
   --
   --  procedure Put_Leg_Index (Item : Leg_Index);
   --
   --  procedure Put (Item : Point_2D);

   --  Active_Trajectory : Trajectory_Information;
   --  Current             : array (Leg_Index) of Point_2D;

   --  procedure Center
   --    (Body_Velocity : Velocity)
   --  is
   --     --  use CGK.Reals;
   --     --  use CGK.Primitives.Transformations_2D;
   --
   --     --  Trajectory_Center   : Point_2D  := Create_Point_2D (0.0, 0.0);
   --     --  Linear_Velocity     : Vector_2D :=
   --     --    Create_Vector_2D (Body_Velocity.X, Body_Velocity.Y);
   --     --  Velocity_Normal     : Vector_2D := -Normal (Linear_Velocity);
   --     --  Delta_Position      : Vector_2D :=
   --     --    Linear_Velocity * Hexapod.Parameters.Control_Cycle.Tick_Duration;
   --     --  Delta_Gamma         : Real      :=
   --     --    Body_Velocity.W * Hexapod.Parameters.Control_Cycle.Tick_Duration;
   --     --  Tick_Transformation : Transformation_2D;
   --     --  Tick_Rotation       : Transformation_2D;
   --     --  Tick_Step           : Transformation_2D;
   --     --  Body_Center         : Point_2D := Create_Point_2D (0.0, 0.0);
   --
   --  begin
   --     Set_Relative_Velocity
   --       (Active_Trajectory, Body_Velocity.X, Body_Velocity.Y, Body_Velocity.W);
   --
   --     --  Put (Real'Image (X (Velocity_Normal)));
   --     --  Put (Real'Image (Y (Velocity_Normal)));
   --     --  New_Line;
   --
   --     --  Translate
   --     --    (Trajectory_Center,
   --     --     Velocity_Normal
   --     --       * (Magnitude (Linear_Velocity) / Body_Velocity.W
   --     --            * Magnitude (Velocity_Normal)));
   --
   --     --  Put (Real'Image (X (Trajectory_Center)));
   --     --  Put (Real'Image (Y (Trajectory_Center)));
   --     --  Put_Length_Image (X (Trajectory_Center));
   --     --  Put ("; ");
   --     --  Put_Length_Image (Y (Trajectory_Center));
   --     --  New_Line;
   --
   --     --  Compute coordinate's transformation at the next tick of the control
   --     --  cycle.
   --
   --     --  Set_Rotation
   --     --    (Active_Trajectory.Tick_Transformation,
   --     --     Trajectory_Center,
   --     --     -Delta_Gamma);
   --
   --     --  Set_Translation (Tick_Transformation, XY (Delta_Position));
   --     --  Rotate (Tick_Transformation, Delta_Gamma);
   --
   --     --  Set_Translation (Tick_Transformation, XY (-Delta_Position));
   --     --  Rotate (Tick_Transformation, -Delta_Gamma);
   --
   --     --  Set_Rotation (Tick_Transformation, -Delta_Gamma);
   --     --  Translate (Tick_Transformation, XY (-Delta_Position));
   --     --
   --     --  --  Set_Rotation (Tick_Rotation, -Delta_Gamma);
   --     --  Set_Rotation (Tick_Rotation, Trajectory_Center, -Delta_Gamma);
   --     --
   --     --  Tick_Step := Tick_Rotation;
   --     --  Multiply (Tick_Step, Tick_Transformation);
   --
   --     --  Radius of trajectory for each leg
   --
   --     --  for Leg in Leg_Index loop
   --     --     declare
   --     --        Workspace_Circle  : Circle_2D :=
   --     --          Standard.Legs.Workspace.Get_Bounded_Circle (Leg);
   --     --        Trajectory_Radius : Real :=
   --     --          Magnitude
   --     --            (Create_Vector_2D
   --     --               (Trajectory_Center, Center (Workspace_Circle)));
   --     --        Trajectory_Circle : Circle_2D :=
   --     --          Create_Circle_2D (Trajectory_Center, Trajectory_Radius);
   --     --        Intersections     : Analytical_Intersection_2D;
   --     --        Point_1           : Point_2D;
   --     --        Point_2           : Point_2D;
   --     --        Vector_1          : Vector_2D;
   --     --        Vector_2          : Vector_2D;
   --     --        Angle_1           : Real;
   --     --        Angle_2           : Real;
   --     --        Angle_12          : Real;
   --     --        Angle_21          : Real;
   --     --        --  AEP               : Point_2D;
   --     --        --  PEP               : Point_2D;
   --     --
   --     --     begin
   --     --        Put_Leg_Index (Leg);
   --     --        Put (" @ ");
   --     --        Put_Length_Image (X (Center (Workspace_Circle)));
   --     --        Put ("; ");
   --     --           Put_Length_Image (Y (Center (Workspace_Circle)));
   --     --        Put ("  R");
   --     --        Put_Length_Image (Trajectory_Radius);
   --     --
   --     --        Intersect (Intersections, Workspace_Circle, Trajectory_Circle);
   --     --
   --     --        if Length (Intersections) /= 2 then
   --     --           --  XXX It is possible when circles are inside each other.
   --     --
   --     --           raise Program_Error;
   --     --
   --     --        else
   --     --           Point_1 := Point (Intersections, 1);
   --     --           Point_2 := Point (Intersections, 2);
   --     --
   --     --           Put ("  (1) ");
   --     --           Put_Length_Image (X (Point_1));
   --     --           Put ("; ");
   --     --           Put_Length_Image (Y (Point_1));
   --     --           Put ("  (2) ");
   --     --           Put_Length_Image (X (Point_2));
   --     --           Put ("; ");
   --     --           Put_Length_Image (Y (Point_2));
   --     --           Put ("  ^ ");
   --     --           --  Put_Length_Image
   --     --           --    (Cross
   --     --           --       (Create_Vector_2D (Trajectory_Center, Point_1),
   --     --           --        Create_Vector_2D (Trajectory_Center, Point_2)));
   --     --           --  Put_Length_Image
   --     --           --    (Dot
   --     --           --       (Create_Vector_2D (Trajectory_Center, Point_1),
   --     --           --        Create_Vector_2D (Trajectory_Center, Point_2)));
   --     --
   --     --           Vector_1 :=
   --     --             Create_Vector_2D (Center (Trajectory_Circle), Point_1);
   --     --           Vector_2 :=
   --     --             Create_Vector_2D (Center (Trajectory_Circle), Point_2);
   --     --
   --     --           Angle_1 := Arctan (X => X (Vector_1), Y => Y (Vector_1));
   --     --           --  Angle_1 := (if @ < 0.0 then 2.0 * PI - @ else @);
   --     --           Angle_2 := Arctan (X => X (Vector_2), Y => Y (Vector_2));
   --     --           --  Angle_2 := (if @ < 0.0 then 2.0 * PI - @ else @);
   --     --
   --     --           Angle_12 := Angle_1 - Angle_2;
   --     --           Angle_12 := (if @ < 0.0 then 2.0 * Pi + @ else @);
   --     --           Angle_12 := Real'Copy_Sign (@, Body_Velocity.W);
   --     --
   --     --           Angle_21 := Angle_2 - Angle_1;
   --     --           Angle_21 := (if @ < 0.0 then 2.0 * Pi + @ else @);
   --     --           Angle_21 := Real'Copy_Sign (@, Body_Velocity.W);
   --     --
   --     --           Put_Angle_Image (Angle_1);
   --     --           Put (" ");
   --     --           Put_Angle_Image (Angle_2);
   --     --           Put (" => ");
   --     --           Put_Angle_Image (Angle_12);
   --     --           Put_Angle_Image (Angle_21);
   --     --
   --     --           Current (Leg) := Point_2;
   --     --        end if;
   --     --
   --     --        New_Line;
   --     --     end;
   --     --  end loop;
   --
   --     --  Put ("Delta gamma ");
   --     --  Put_Angle_Image (Delta_Gamma);
   --     --  New_Line;
   --     --
   --     --  Put (Body_Center);
   --     --  Put (" |");
   --
   --     --  for Leg in Leg_Index loop
   --     --     Put (Current (Leg));
   --     --  end loop;
   --
   --     --  New_Line;
   --
   --     --  for J in 1 .. 10 loop
   --     --     --  Transform (Body_Center, Tick_Rotation);
   --     --     --  Transform (Body_Center, Tick_Transformation);
   --     --     Transform (Body_Center, Active_Trajectory.Tick_Transformation);
   --     --     Put (Body_Center);
   --     --     --  Put (" |");
   --     --
   --     --     for Leg in Leg_Index loop
   --     --        Transform (Current (Leg), Active_Trajectory.Tick_Transformation);
   --     --        --  Transform (Current (Leg), Tick_Rotation);
   --     --        --  Transform (Current (Leg), Tick_Transformation);
   --     --
   --     --        --  Transform (Current (Leg), Tick_Step);
   --     --
   --     --        Put (Current (Leg));
   --     --     end loop;
   --     --
   --     --     New_Line;
   --     --  end loop;
   --  end Center;

   -------------------------------
   -- Anterior_Extreme_Position --
   -------------------------------

   function Anterior_Extreme_Position
     (Self : Trajectory_Information;
      Leg  : Leg_Index) return CGK.Primitives.Points_2D.Point_2D is
   begin
      return Self.Leg_Information (Leg).AEP;
   end Anterior_Extreme_Position;

   --------------------------------------
   -- Get_Bodypath_Tick_Transformation --
   --------------------------------------

   function Get_Bodypath_Tick_Transformation
     (Self : Trajectory_Information)
      return CGK.Primitives.Transformations_3D.Transformation_3D is
   begin
      return Self.Tick_Transformation_3;
   end Get_Bodypath_Tick_Transformation;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   --  procedure Tick;
   --  --  Computes gait and updates legs' trajectory when necessary.
   --
   --  procedure Set_Velocity
   --    (VX : CGK.Reals.Real;
   --     VY : CGK.Reals.Real);
   --  --  Sets desired velocity
   --
   --  procedure Print_Workspace is
   --  begin
   --     for Leg in Leg_Index loop
   --        declare
   --           Workspace_Circle  : Circle_2D :=
   --             Standard.Legs.Workspace.Get_Bounded_Circle (Leg);
   --
   --        begin
   --           Put_Leg_Index (Leg);
   --           Put (" @ ");
   --           Put_Length_Image (X (Center (Workspace_Circle)));
   --           Put ("; ");
   --              Put_Length_Image (Y (Center (Workspace_Circle)));
   --           Put ("  R");
   --           Put_Length_Image (Radius (Workspace_Circle));
   --           New_Line;
   --        end;
   --     end loop;
   --
   --     New_Line;
   --  end Print_Workspace;
   --
   --  procedure Put (Item : Point_2D) is
   --  begin
   --     Put_Length_Image (X (Item));
   --     Put ("; ");
   --     Put_Length_Image (Y (Item));
   --  end Put;
   --
   --  procedure Put_Angle_Image (Item : CGK.Reals.Real) is
   --     Buffer : String (1 .. 6);
   --
   --  begin
   --     Real_IO.Put (To => Buffer, Item => Item, Aft => 3, Exp => 0);
   --     Put (Buffer);
   --  end Put_Angle_Image;
   --
   --  procedure Put_Leg_Index (Item : Leg_Index) is
   --  begin
   --     Put
   --       (case Item is
   --           when Left_Front => "<^",
   --           when Left_Middle => "<-",
   --           when Left_Rear => "<v",
   --           when Right_Front => "^>",
   --           when Right_Middle => "->",
   --           when Right_Rear => "v>");
   --  end Put_Leg_Index;
   --
   --  procedure Put_Length_Image (Item : CGK.Reals.Real) is
   --     Buffer : String (1 .. 8);
   --
   --  begin
   --     Real_IO.Put (To => Buffer, Item => Item, Aft => 3, Exp => 0);
   --     Put (Buffer);
   --  end Put_Length_Image;

   ----------------
   -- Is_Forward --
   ----------------

   function Is_Forward
     (Path  : CGK.Primitives.Lines_2D.Line_2D;
      Point : CGK.Primitives.Points_2D.Point_2D) return Boolean
   is
      use type CGK.Primitives.XYs.XY;
      use CGK.Primitives.Directions_2D;

      C : constant CGK.Primitives.XYs.XY := XY (Point) - XY (Location (Path));

   begin
      return CGK.Primitives.XYs.Dot_Product (C, XY (Direction (Path))) >= 0.0;
   end Is_Forward;

   ---------------------
   -- Remaining_Ticks --
   ---------------------

   function Remaining_Ticks
     (Self      : Trajectory_Information;
      Workspace : CGK.Primitives.Circles_2D.Circle_2D;
      Position  : CGK.Primitives.Points_2D.Point_2D) return Natural
   is
      use CGK.Reals;

      Path          : Line_2D;
      Translation   : Vector_2D;
      Builder       : Direction_2D_Builder;
      Intersections : Analytical_Intersection_2D;
      Point_1       : Point_2D;
      Point_2       : Point_2D;
      Forward_1     : Boolean;
      Forward_2     : Boolean;
      Length_1      : Real;
      Length_2      : Real;

   begin
      if Is_Identity (Self.Tick_Transformation) then
         return 0;

      elsif Is_Translation (Self.Tick_Transformation) then
         Translation :=
           Create_Vector_2D (Translation_Component (Self.Tick_Transformation));

         --  Compute path's line

         Build (Builder, XY (Translation));
         Path := Create_Line_2D (Position, Direction (Builder));

         --  Compute intersections of path line with workspace circle

         Intersect (Intersections, Path, Workspace);

         if Length (Intersections) /= 2 then
            --  Path from the current position doesn't intersects with
            --  workspace area.

            return 0;

         else
            Point_1   := Point (Intersections, 1);
            Point_2   := Point (Intersections, 2);
            Forward_1 := Is_Forward (Path, Point_1);
            Forward_2 := Is_Forward (Path, Point_2);
            Length_1  := Magnitude (Create_Vector_2D (Position, Point_1));
            Length_2  := Magnitude (Create_Vector_2D (Position, Point_2));

            if Forward_1 and Forward_2 then
               --  There are two intersections of the path with workspace.
               --  Current position is outside of the workspace area.

               if Length_1 > Length_2 then
                  return
                    Natural (Real'Floor (Length_1 / Magnitude (Translation)));

               else
                  raise Program_Error;
               end if;

            elsif Forward_1 then
               return
                 Natural (Real'Floor (Length_1 / Magnitude (Translation)));

            elsif Forward_2 then
               raise Program_Error;

            else
               --  Close to workspace edge, but outside of it.

               return 0;
            end if;
         end if;

      else
         declare
            --     Workspace_Circle  : Circle_2D :=
            --       Standard.Legs.Workspace.Get_Bounded_Circle (Leg);
            Trajectory_Radius : constant Real :=
              Magnitude (Create_Vector_2D (Self.Trajectory_Center, Position));
            Trajectory_Circle : constant Circle_2D :=
              Create_Circle_2D (Self.Trajectory_Center, Trajectory_Radius);
            --     Intersections     : Analytical_Intersection_2D;
            --     Point_1           : Point_2D;
            --     Point_2           : Point_2D;
            Vector_P          : Vector_2D;
            Vector_1          : Vector_2D;
            Vector_2          : Vector_2D;
            Angle_1           : Real;
            Angle_2           : Real;
            --     --  Angle_12          : Real;
            --     --  Angle_21          : Real;
            --     --  AEP               : Point_2D;
            --     --  PEP               : Point_2D;
            --
         begin
            --  Compute intersections of the trajectory circle with the
            --  workspace circle.

            Intersect (Intersections, Workspace, Trajectory_Circle);

            if Length (Intersections) = 0 then
               if Is_Disjoint_Elements (Intersections) then
                  --  Workspace and trajectory are disjoints.
                  --
                  --  XXX Why is it appear? Computation error?

                  Debug.Log.Put_Line ("Disjoint");

                  return 0;

               else
                  if Radius (Workspace) >= Trajectory_Radius then
                     --  Trajectory is inside workspace area.

                     Debug.Log.Put_Line ("Inside");

                     return Natural'Last;

                  else
                     --  Trajectory is outside of the workspace.
                     --
                     --  XXX Why is it appear? Computation error?

                     return 0;
                  end if;
               end if;

            elsif Length (Intersections) = 1 then
               if Is_Disjoint_Elements (Intersections) then
                  raise Program_Error;

               else
                  --  XXX It is possible when circles are inside each other.

                  raise Program_Error;
                  --  return Natural'Last;
               end if;

            else
               Point_1 := Point (Intersections, 1);
               Point_2 := Point (Intersections, 2);

               Vector_P :=
                 Create_Vector_2D (Center (Trajectory_Circle), Position);
               Vector_1 :=
                 Create_Vector_2D (Center (Trajectory_Circle), Point_1);
               Vector_2 :=
                 Create_Vector_2D (Center (Trajectory_Circle), Point_2);

               Angle_1 :=
                 CGK.Reals.Elementary_Functions.Arctan
                   (X => Dot (Vector_P, Vector_1),
                    Y => Cross (Vector_P, Vector_1));
               Angle_2 :=
                 CGK.Reals.Elementary_Functions.Arctan
                   (X => Dot (Vector_P, Vector_2),
                    Y => Cross (Vector_P, Vector_2));

               --  Debug.Log.Put_Line
               --    (Real'Image (Angle_1)
               --         & Real'Image (Angle_2));

               if Self.Angular_Velocity > 0.0 then
                  if Angle_1 > 0.0 then
                     return 0;

                  else
                     return
                       Natural
                         (CGK.Reals.Real'Floor
                            (-Angle_1 /
                               (Self.Angular_Velocity
                                * Hexapod.Parameters.Control_Cycle.Tick_Duration)));
                  end if;

               else
                  if Angle_2 < 0.0 then
                     return 0;

                  else
                     return
                       Natural
                         (CGK.Reals.Real'Floor
                            (-Angle_2 /
                               (Self.Angular_Velocity
                                * Hexapod.Parameters.Control_Cycle.Tick_Duration)));
                  end if;
               end if;
            --     --  Angle_1 := Arctan (X => X (Vector_1), Y => Y (Vector_1));
            --     --  --  Angle_1 := (if @ < 0.0 then 2.0 * PI - @ else @);
            --     --  Angle_2 := Arctan (X => X (Vector_2), Y => Y (Vector_2));
            --     --  --  Angle_2 := (if @ < 0.0 then 2.0 * PI - @ else @);
            --     --
            --     --  Angle_12 := Angle_1 - Angle_2;
            --     --  Angle_12 := (if @ < 0.0 then 2.0 * Pi + @ else @);
            --     --  Angle_12 := Real'Copy_Sign (@, Absolute_Angular_Velocity);
            --     --
            --     --  Angle_21 := Angle_2 - Angle_1;
            --     --  Angle_21 := (if @ < 0.0 then 2.0 * Pi + @ else @);
            --     --  Angle_21 := Real'Copy_Sign (@, Absolute_Angular_Velocity);
            --
            --     --  Put_Angle_Image (Angle_1);
            --     --  Put (" ");
            --     --  Put_Angle_Image (Angle_2);
            --     --  Put (" => ");
            --     --  Put_Angle_Image (Angle_12);
            --     --  Put_Angle_Image (Angle_21);
            --
            --        Self.Leg_Information (Leg).AEP := Point_2;
            --     --  Current (Leg) := Point_2;

               --  raise Program_Error;
            end if;
         end;
      end if;
   end Remaining_Ticks;

   ---------------------------
   -- Set_Relative_Velocity --
   ---------------------------

   procedure Set_Relative_Velocity
     (Self       : out Trajectory_Information;
      Velocity_X : CGK.Reals.Real;
      Velocity_Y : CGK.Reals.Real;
      Velocity_W : CGK.Reals.Real)
   is
      use type CGK.Mathematics.Vectors_3.Vector_3;
      use type CGK.Primitives.XYs.XY;
      use CGK.Primitives.Transformations_3D;
      use CGK.Reals;

      function Normalize_Linear_Velocity return Vector_2D;

      -------------------------------
      -- Normalize_Linear_Velocity --
      -------------------------------

      function Normalize_Linear_Velocity return Vector_2D is
         Result : Vector_2D     :=
           Create_Vector_2D (X => Velocity_X, Y => Velocity_Y);
         Length : constant CGK.Reals.Real := Magnitude (Result);
         Scale  : CGK.Reals.Real;

      begin
         if Velocity_X = 0.0 and Velocity_Y = 0.0 then
            Scale := 1.0;

         elsif abs Velocity_X >= abs Velocity_Y then
            Scale := abs Velocity_X / Length;

         else
            Scale := abs Velocity_Y / Length;
         end if;

         Result := @ * Scale;

         return Result;
      end Normalize_Linear_Velocity;

      Linear_Velocity        : constant Vector_2D := Normalize_Linear_Velocity;
      Linear_Speed           : constant Real :=
        Magnitude (Linear_Velocity);

      --  Linear_Velocity_Normal : Vector_2D          := -Normal (Linear_Velocity);
      --  Speed           : constant Real :=
      --    Sqrt (Velocity_X * Velocity_X + Velocity_Y * Velocity_Y);
      --  Linear_Velocity : constant Vector_2D :=
      --    (if Speed = 0.0
      --       then Create_Vector_2D (0.0, 0.0)
      --       else Create_Vector_2D (Velocity_X, Velocity_Y) / Speed);

      Max_Radius      : constant Real := 0.220 + 0.082;
      Max_Step        : constant Real := 0.082;
      --  XXX Must be computed according to the current workspace
      --  configuration.

      Absolute_Linear_Velocity : constant Vector_2D :=
        Linear_Velocity * Max_Step;

      --  Velocity_Normal   : Vector_2D := -Normal (Linear_Velocity);
      --  Trajectory_Center_Vector :=
      --  Trajectory_Center : Point_2D  := Create_Point_2D (0.0, 0.0);

   begin
      --  Put ("Wmax");
      --  Put_Length_Image (Max_Step / Max_Radius);
      --  New_Line;
      --
      --  Put_Length_Image (X (Linear_Velocity));
      --  Put_Length_Image (Y (Linear_Velocity));
      --  Put_Length_Image (Velocity_W);
      --  Put (" (");
      --  Put_Length_Image (X (Absolute_Linear_Velocity));
      --  Put_Length_Image (Y (Absolute_Linear_Velocity));
      --  --  Put_Length_Image (X (Linear_Velocity) * Max_Step);
      --  --  Put_Length_Image (Y (Linear_Velocity) * Max_Step);
      --  --  Put_Length_Image ((Max_Step - Magnitude (Linear_Velocity) * Max_Step) / Max_Radius);

      if Velocity_W = 0.0 then
         Self.Angular_Velocity  := 0.0;
         Self.Trajectory_Center := Create_Point_2D (0.0, 0.0);
         --  Reset trajection center to origin of coordinate system for
         --  convenience.

         if X (Absolute_Linear_Velocity) = 0.0
           and Y (Absolute_Linear_Velocity) = 0.0
         then
            Set_Identity (Self.Tick_Transformation);
            Set_Identity (Self.Tick_Transformation_3);

            for Leg in Leg_Index loop
               Self.Leg_Information (Leg).AEP :=
                 Center (Standard.Legs.Workspace.Get_Bounded_Circle (Leg));
            end loop;

         else
            Set_Translation
              (Self.Tick_Transformation,
               -CGK.Primitives.Vectors_2D.XY (Absolute_Linear_Velocity)
                 * Hexapod.Parameters.Control_Cycle.Tick_Duration);
            Set_Translation
              (Self.Tick_Transformation_3,
               CGK.Primitives.XYZs.As_XYZ
                 (-CGK.Mathematics.Vectors_3.Vector_3'
                      (0 => X (Absolute_Linear_Velocity),
                       1 => Y (Absolute_Linear_Velocity),
                       2 => 0.0)
                  * Hexapod.Parameters.Control_Cycle.Tick_Duration));

            for Leg in Leg_Index loop
               declare
                  Workspace        : constant Circle_2D :=
                    Standard.Legs.Workspace.Get_Bounded_Circle (Leg);
                  Workspace_Center : constant Point_2D := Center (Workspace);
                  Builder          : Direction_2D_Builder;
                  Path             : Line_2D;
                  Intersections    : Analytical_Intersection_2D;
                  Point_1          : Point_2D;
                  Point_2          : Point_2D;
                  Forward_1        : Boolean;
                  Forward_2        : Boolean;

               begin
                  --  Compute path's line

                  Build (Builder, XY (Absolute_Linear_Velocity));
                  Path :=
                    Create_Line_2D (Workspace_Center, Direction (Builder));

                  --  Compute intersections of path line with workspace circle

                  Intersect (Intersections, Path, Workspace);
                  pragma Assert (Length (Intersections) = 2);
                  --  Workspace area intersects with straight line path in two
                  --  points.

                  Point_1   := Point (Intersections, 1);
                  Point_2   := Point (Intersections, 2);
                  Forward_1 := Is_Forward (Path, Point_1);
                  Forward_2 := Is_Forward (Path, Point_2);

                  pragma Assert (Forward_1 xor Forward_2);
                  --  Only one of the forward flags are True by construction.

                  if Forward_1 then
                     Self.Leg_Information (Leg).AEP := Point_1;

                  elsif Forward_2 then
                     raise Program_Error;

                  else
                     raise Program_Error;
                  end if;
               end;
            end loop;
         end if;

      else
         Self.Angular_Velocity := Max_Step / Max_Radius * Velocity_W;

         if Linear_Speed = 0.0 then
            Self.Trajectory_Center := Create_Point_2D (0.0, 0.0);

         else
            Self.Trajectory_Center :=
              Create_Point_2D
                (XY
                   (-Normal (Absolute_Linear_Velocity)
                       / Self.Angular_Velocity));
         end if;

         --  Put_Length_Image (X (Trajectory_Center));
         --  Put_Length_Image (Y (Trajectory_Center));
         --  Put_Length_Image (-Absolute_Angular_Velocity
         --                    * Hexapod.Parameters.Control_Cycle.Tick_Duration);

         declare
            use CGK.Primitives.XYZs;

            Angle  : constant Real :=
              -Self.Angular_Velocity
                * Hexapod.Parameters.Control_Cycle.Tick_Duration;
            Point  : constant CGK.Mathematics.Vectors_3.Vector_3 :=
              -CGK.Mathematics.Vectors_3.Vector_3'
                 (0 => X (Self.Trajectory_Center),
                  1 => Y (Self.Trajectory_Center),
                  2 => 0.0);
            Offset : CGK.Primitives.XYZs.XYZ;

         begin
            --  2D rotation

            Set_Rotation
              (Self.Tick_Transformation,
               Self.Trajectory_Center,
               Angle);

            --  3D rotation

            Set_Rotation_Z (Self.Tick_Transformation_3, Angle);

            Offset := Transform (Self.Tick_Transformation_3, As_XYZ (Point));
            Offset := @ - As_XYZ (Point);
            Translate (Self.Tick_Transformation_3, Offset);
         end;

      --  Put_Length_Image (Absolute_Angular_Velocity);

      --  New_Line;

         for Leg in Leg_Index loop
            declare
               Workspace_Circle  : constant Circle_2D :=
                 Standard.Legs.Workspace.Get_Bounded_Circle (Leg);
               Trajectory_Radius : constant Real :=
                 Magnitude
                   (Create_Vector_2D
                      (Self.Trajectory_Center, Center (Workspace_Circle)));
               Trajectory_Circle : constant Circle_2D :=
                 Create_Circle_2D (Self.Trajectory_Center, Trajectory_Radius);
               Intersections     : Analytical_Intersection_2D;
               Point_1           : Point_2D;
               Point_2           : Point_2D;
               --  Vector_1          : Vector_2D;
               --  Vector_2          : Vector_2D;
               --  Angle_1           : Real;
               --  Angle_2           : Real;
               --  Angle_12          : Real;
               --  Angle_21          : Real;
               --  AEP               : Point_2D;
               --  PEP               : Point_2D;

            begin
            --  Put_Leg_Index (Leg);
            --  Put (" @ ");
            --  Put_Length_Image (X (Center (Workspace_Circle)));
            --  Put ("; ");
            --     Put_Length_Image (Y (Center (Workspace_Circle)));
            --  Put ("  R");
            --  Put_Length_Image (Trajectory_Radius);

               Intersect (Intersections, Workspace_Circle, Trajectory_Circle);

               if Length (Intersections) /= 2 then
                  --  XXX It is possible when circles are inside each other.

                  Self.Leg_Information (Leg).AEP := Center (Workspace_Circle);
                  --  Set AEP to center of the workspace, to have some point
                  --  defined inside the workspace area.

               else
                  Point_1 := Point (Intersections, 1);
                  Point_2 := Point (Intersections, 2);

               --  Put ("  (1) ");
               --  Put_Length_Image (X (Point_1));
               --  Put ("; ");
               --  Put_Length_Image (Y (Point_1));
               --  Put ("  (2) ");
               --  Put_Length_Image (X (Point_2));
               --  Put ("; ");
               --  Put_Length_Image (Y (Point_2));
               --  Put ("  ^ ");
               --  --  Put_Length_Image
               --  --    (Cross
               --  --       (Create_Vector_2D (Trajectory_Center, Point_1),
               --  --        Create_Vector_2D (Trajectory_Center, Point_2)));
               --  --  Put_Length_Image
               --  --    (Dot
               --  --       (Create_Vector_2D (Trajectory_Center, Point_1),
               --  --        Create_Vector_2D (Trajectory_Center, Point_2)));

               --  Vector_1 :=
               --    Create_Vector_2D (Center (Trajectory_Circle), Point_1);
               --  Vector_2 :=
               --    Create_Vector_2D (Center (Trajectory_Circle), Point_2);

               --  Angle_1 := Arctan (X => X (Vector_1), Y => Y (Vector_1));
               --  --  Angle_1 := (if @ < 0.0 then 2.0 * PI - @ else @);
               --  Angle_2 := Arctan (X => X (Vector_2), Y => Y (Vector_2));
               --  --  Angle_2 := (if @ < 0.0 then 2.0 * PI - @ else @);
               --
               --  Angle_12 := Angle_1 - Angle_2;
               --  Angle_12 := (if @ < 0.0 then 2.0 * Pi + @ else @);
               --  Angle_12 := Real'Copy_Sign (@, Absolute_Angular_Velocity);
               --
               --  Angle_21 := Angle_2 - Angle_1;
               --  Angle_21 := (if @ < 0.0 then 2.0 * Pi + @ else @);
               --  Angle_21 := Real'Copy_Sign (@, Absolute_Angular_Velocity);

               --  Put_Angle_Image (Angle_1);
               --  Put (" ");
               --  Put_Angle_Image (Angle_2);
               --  Put (" => ");
               --  Put_Angle_Image (Angle_12);
               --  Put_Angle_Image (Angle_21);

                  if Self.Angular_Velocity > 0.0 then
                     Self.Leg_Information (Leg).AEP := Point_2;

                  else
                     Self.Leg_Information (Leg).AEP := Point_1;
                  end if;
               end if;

            --  New_Line;
            end;
         end loop;
      end if;
   end Set_Relative_Velocity;

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (Self  : Trajectory_Information;
      Point : in out CGK.Primitives.Points_2D.Point_2D) is
   begin
      Transform (Point, Self.Tick_Transformation);
   end Transform;

end Legs.Trajectory;
