--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  pragma Restrictions (No_Elaboration_Code);

with Ada.Numerics;

with CGK.Primitives.Analytical_Intersections_2D;
with CGK.Primitives.Circles_2D;
with CGK.Primitives.Directions_2D.Builders;
with CGK.Primitives.Points_2D.Containers;
with CGK.Primitives.Lines_2D;
with CGK.Primitives.Vectors_2D;

with Legs.State;
with Legs.Trajectory_Generator;
with Legs.Workspace;

package body Legs.Gait_Generator is

   use CGK.Primitives.Analytical_Intersections_2D;
   use CGK.Primitives.Circles_2D;
   use CGK.Primitives.Directions_2D;
   use CGK.Primitives.Directions_2D.Builders;
   use CGK.Primitives.Lines_2D;
   use CGK.Primitives.Points_2D;
   use CGK.Primitives.Vectors_2D;
   use CGK.Reals;
   use Standard.Legs.State;

   use type CGK.Primitives.Points_2D.Containers.Point_2D_Array_Count;

   Control_Loop_Frequency : constant := 100.0;
   Control_Tick_Duration  : constant := 1.0 / Control_Loop_Frequency;
   Swing_Ticks            : constant := 50;
   --  Parameters of the control loop.
   --  XXX Must be shared between all modules.

   type Leg_State_Kind is (Stance, Swing);

   type Leg_State (Kind : Leg_State_Kind := Stance) is record
      case Kind is
         when Stance =>
            PEP_Tick : Natural;

         when Swing =>
            AEP_Tick : Natural;
      end case;
   end record;

   --  Linear_Tolerance  : constant := 0.0001;
   Angular_Tolerance : constant := Ada.Numerics.Pi / 512.0;

   Current_Tick : Natural := 0;
   --  Number of the current tick.

   Velocity_X         : CGK.Reals.Real := 0.0;
   Velocity_Y         : CGK.Reals.Real := 0.0;
   Velocity_Direction : CGK.Primitives.Directions_2D.Direction_2D;
   Velocity_Vector    : CGK.Primitives.Vectors_2D.Vector_2D;
   Velocity_Changed   : Boolean        := False;
   --  Velocity of the body

   State : array (Leg_Index) of Leg_State;

   function Is_Forward
     (Current : Point_2D;
      Point   : Point_2D) return Boolean;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Current_Tick     := 0;
      Velocity_X       := 0.0;
      Velocity_Y       := 0.0;
      Velocity_Changed := False;

      for Leg in Leg_Index loop
         State (Leg) :=
           (Kind     => Stance,
            PEP_Tick => Natural'Last);
            --  Path_AEP => <>,
            --  Path_PEP => <>);
      end loop;
   end Initialize;

   --------------
   -- Is_Ahead --
   --------------

   function Is_Ahead (Leg : Leg_Index) return Boolean is
      Prev : constant Leg_Index :=
        (if Leg = Leg_Index'First then Leg_Index'Last else Leg_Index'Pred (Leg));
      Next : constant Leg_Index :=
        (if Leg = Leg_Index'Last then Leg_Index'First else Leg_Index'Succ (Leg));

   begin
      if State (Leg).Kind = Swing then
         --  Leg in swing state is always ahead, thus it locks neighboring legs

         return True;
      end if;

      if State (Prev).Kind = Swing or State (Next).Kind = Swing then
         --  One of neighboring legs is in swing, leg is behind

         return False;
      end if;

      if State (Prev).PEP_Tick > State (Leg).PEP_Tick
        and State (Leg).PEP_Tick < State (Next).PEP_Tick
      then
         --  Leg is close to PEP than neighboring legs

         return True;
      end if;

      if State (Prev).PEP_Tick = State (Leg).PEP_Tick
        and State (Leg).PEP_Tick = State (Next).PEP_Tick
      then
         --  Leg and its neighboring legs are equal to PEP, select one of the
         --  leg.

         return Leg_Index'Pos (Leg) mod 2 = 1;
      end if;

      return False;
   end Is_Ahead;

   ----------------
   -- Is_Forward --
   ----------------

   function Is_Forward
     (Current : Point_2D;
      Point   : Point_2D) return Boolean
   is
      Builder : Direction_2D_Builder;

   begin
      Build (Builder, Current, Point);

      return
        Is_Equal (Velocity_Direction, Direction (Builder), Angular_Tolerance);
   end Is_Forward;

   ------------------
   -- Set_Velocity --
   ------------------

   procedure Set_Velocity
     (VX : CGK.Reals.Real;
      VY : CGK.Reals.Real)
   is
      Builder : Direction_2D_Builder;

   begin
      if VX /= Velocity_X or VY /= Velocity_Y then
         Velocity_X       := -VX;
         Velocity_Y       := -VY;
         Velocity_Vector  := Create_Vector_2D (-VX, -VY);
         Velocity_Changed := True;

         if Velocity_X /= 0.0 or Velocity_Y /= 0.0 then
            Build (Builder, Velocity_X, Velocity_Y);
            Velocity_Direction := Direction (Builder);
         end if;
      end if;
   end Set_Velocity;

   --------------------
   -- Compute_Linear --
   --------------------

   procedure Compute_Linear
     (Leg : Leg_Index)
   is
      procedure Update_State (Length  : CGK.Reals.Real);

      ------------------
      -- Update_State --
      ------------------

      procedure Update_State (Length  : CGK.Reals.Real) is
         DL       : constant Reals.Real :=
           Magnitude (Velocity_Vector) * Control_Tick_Duration;
         T        : constant Natural := Natural (Real'Floor (Length / DL));

      begin
         Standard.Legs.Trajectory_Generator.Set_Linear
           (Leg,
            Velocity_X * Control_Tick_Duration,
            Velocity_Y * Control_Tick_Duration);
         State (Leg) :=
           (Kind     => Stance,
            PEP_Tick => Current_Tick + T);
      end Update_State;

      Current       : constant Point_2D :=
        Create_Point_2D
          (Kinematics.X (Position (Leg)), Kinematics.Y (Position (Leg)));
      Path          : Line_2D;
      Workspace     : Circle_2D;
      Intersections : Analytical_Intersection_2D;
      Point_1       : Point_2D;
      Point_2       : Point_2D;
      Forward_1     : Boolean;
      Forward_2     : Boolean;
      Length_1      : Real;
      Length_2      : Real;

   begin
      --  Compute bounded workspace.

      Standard.Legs.Workspace.Get_Bounding_Shape (Leg, Workspace);
      Workspace :=
        Create_Circle_2D (Center (Workspace), Radius (Workspace) / 2.0);

      --  Special case to shutdown on stop

      if Velocity_X = 0.0 and Velocity_Y = 0.0 then
         Standard.Legs.Trajectory_Generator.Set_Linear (Leg, 0.0, 0.0);

         if Current = Center (Workspace) then
            State (Leg) :=
              (Kind     => Stance,
               PEP_Tick => Natural'Last);

         else
            State (Leg) :=
              (Kind     => Stance,
               PEP_Tick => Current_Tick);
         end if;

         return;
      end if;

      --  Compute path line

      Path := Create_Line_2D (Current, Velocity_Direction);

      --  Compute intersections of path line with workspace circle

      Intersect (Intersections, Path, Workspace);

      if Length (Intersections) /= 2 then
         --  Path from the current position doesn't intersects with workspace
         --  area.

         Standard.Legs.Trajectory_Generator.Set_Linear
           (Leg,
            Velocity_X * Control_Tick_Duration,
            Velocity_Y * Control_Tick_Duration);
         State (Leg) :=
           (Kind     => Stance,
            PEP_Tick => Current_Tick);

      else
         Point_1   := Point (Intersections, 1);
         Point_2   := Point (Intersections, 2);
         Forward_1 := Is_Forward (Current, Point_1);
         Forward_2 := Is_Forward (Current, Point_2);
         Length_1  := Magnitude (Create_Vector_2D (Current, Point_1));
         Length_2  := Magnitude (Create_Vector_2D (Current, Point_2));

         if Forward_1 and Forward_2 then
            --  There are two intersections of the path with workspace are.
            --  Current position is outside of the workspace are.

            if Length_1 > Length_2 then
               Update_State (Length_1);

            else
               raise Program_Error;
            end if;

         elsif Forward_1 then
            Update_State (Length_1);

         elsif Forward_2 then
            Update_State (Length_2);

         else
            --  Close to workspace edge, but outside of it.

            Standard.Legs.Trajectory_Generator.Set_Linear
              (Leg,
               Velocity_X * Control_Tick_Duration,
               Velocity_Y * Control_Tick_Duration);
            State (Leg) :=
              (Kind     => Stance,
               PEP_Tick => Current_Tick);
         end if;
      end if;
   end Compute_Linear;

   -----------------
   -- Start_Swing --
   -----------------

   function Start_Swing (Leg : Leg_Index) return Boolean is
      Prev : constant Leg_Index :=
        (if Leg = Leg_Index'First then Leg_Index'Last else Leg_Index'Pred (Leg));
      Next : constant Leg_Index :=
        (if Leg = Leg_Index'Last then Leg_Index'First else Leg_Index'Succ (Leg));

   begin
      return
        State (Leg).PEP_Tick <= Current_Tick
          or else State (Prev).PEP_Tick <= Current_Tick + Swing_Ticks
          or else State (Next).PEP_Tick <= Current_Tick + Swing_Ticks;
   end Start_Swing;

   -------------------------------
   -- Compute_Extreme_Positions --
   -------------------------------

   procedure Compute_Extreme_Positions
     (Leg : Leg_Index;
      AEP : out CGK.Primitives.Points_2D.Point_2D)
   is
      Path             : Line_2D;
      Workspace        : Circle_2D;
      Workspace_Center : Point_2D;
      Intersections    : Analytical_Intersection_2D;
      Point_1          : Point_2D;
      Point_2          : Point_2D;
      Forward_1        : Boolean;
      Forward_2        : Boolean;
      Length_1         : Real;
      Length_2         : Real;

   begin
      --  Compute bounded workspace.

      Standard.Legs.Workspace.Get_Bounding_Shape (Leg, Workspace);
      Workspace        :=
        Create_Circle_2D (Center (Workspace), Radius (Workspace) / 2.0);
      Workspace_Center := Center (Workspace);

      --  Special case to shutdown on stop

      if Velocity_X = 0.0 and Velocity_Y = 0.0 then
         Standard.Legs.Trajectory_Generator.Set_Linear (Leg, 0.0, 0.0);
         AEP := Workspace_Center;

         return;
      end if;

      --  Compute path line

      Path := Create_Line_2D (Workspace_Center, Velocity_Direction);

      --  Compute intersections of path line with workspace circle

      Intersect (Intersections, Path, Workspace);

      if Length (Intersections) /= 2 then

         raise Program_Error;

      else
         Point_1   := Point (Intersections, 1);
         Point_2   := Point (Intersections, 2);
         Forward_1 := Is_Forward (Workspace_Center, Point_1);
         Forward_2 := Is_Forward (Workspace_Center, Point_2);
         Length_1  := Magnitude (Create_Vector_2D (Workspace_Center, Point_1));
         Length_2  := Magnitude (Create_Vector_2D (Workspace_Center, Point_2));

         if Forward_1 and Forward_2 then
            --  There are two intersections of the path with workspace are.
            --  Current position is outside of the workspace are.

            if Length_1 > Length_2 then
               raise Program_Error;

            else
               raise Program_Error;
            end if;

         elsif Forward_1 then
            AEP := Point_2;

         elsif Forward_2 then
            raise Program_Error;

         else
            raise Program_Error;
         end if;
      end if;
   end Compute_Extreme_Positions;

   ----------
   -- Tick --
   ----------

   procedure Tick is
   begin
      --  Update state of the landed legs.

      for Leg in Leg_Index loop
         if State (Leg).Kind = Swing
           and then State (Leg).AEP_Tick <= Current_Tick
         then
            Compute_Linear (Leg);
         end if;
      end loop;

      if Velocity_Changed then
         --  Velocity has been changed, recompute path of legs in stance.

         for Leg in Leg_Index loop
            if State (Leg).Kind = Stance then
               Compute_Linear (Leg);
            end if;
         end loop;

         Velocity_Changed := False;
      end if;

      for Leg in Leg_Index'Range loop
         if Is_Ahead (Leg) then
            if State (Leg).Kind = Stance then
               if Start_Swing (Leg) then
                  declare
                     AEP : Point_2D;

                  begin
                     Compute_Extreme_Positions (Leg, AEP);

                     Standard.Legs.Trajectory_Generator.Set_Swing
                       (Leg    => Leg,
                        AEP_X  => X (AEP),
                        AEP_Y  => Y (AEP),
                        Height => 0.030);
                     State (Leg) :=
                       (Kind     => Swing,
                        AEP_Tick => Current_Tick + Swing_Ticks);
                  end;
               end if;

            else
               if State (Leg).AEP_Tick <= Current_Tick then
                  raise Program_Error;
               end if;
            end if;
         end if;
      end loop;

      Current_Tick := @ + 1;
   end Tick;

end Legs.Gait_Generator;
