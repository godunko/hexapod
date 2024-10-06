--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  pragma Restrictions (No_Elaboration_Code);

with CGK.Primitives.Circles_2D;
with CGK.Primitives.Points_2D;
with CGK.Primitives.XYs;

with Debug.Log;
with Legs.State;
with Legs.Trajectory;
with Legs.Trajectory_Generator;
with Legs.Workspace;

package body Legs.Gait_Generator is

   use CGK.Primitives.Circles_2D;
   use CGK.Primitives.Points_2D;
   use CGK.Reals;
   use Standard.Legs.State;

   Swing_Ticks            : constant := 50;
   --  Parameters of the control loop.
   --  XXX Must be shared between all modules.

   type Leg_State_Kind is (Stance, Swing);

   type Leg_State (Kind : Leg_State_Kind := Stance) is record
      case Kind is
         when Stance =>
            PEP_Tick  : Natural;
            MPEP_Tick : Natural;

         when Swing =>
            AEP_Tick : Natural;
      end case;
   end record;

   Current_Tick : Natural := 0;
   --  Number of the current tick.

   type Velocity_Information is record
      RVX        : CGK.Reals.Real := 0.0;
      RVY        : CGK.Reals.Real := 0.0;
      RVW        : CGK.Reals.Real := 0.0;

      Trajectory : aliased Standard.Legs.Trajectory.Trajectory_Information;
   end record;

   Velocity           : array (Boolean) of Velocity_Information;
   Velocity_Bank      : Boolean := False;
   Velocity_Changed   : Boolean := False;
   --  Velocity of the body.
   --
   --  XXX Two banks of parameters are used to minimize probability of race
   --  condition, need to be rewritten.

   State : array (Leg_Index) of Leg_State;

   function Is_Identical
     (Point_1 : Point_2D; Point_2 : Point_2D) return Boolean;
   function Is_Identical
     (Point : Point_2D; Position : Kinematics.Position) return Boolean;
   --  Returns True when given points are closer that 0.001 m to each other.

   --------------------
   -- Compute_Linear --
   --------------------

   procedure Compute_Linear
     (Leg : Leg_Index)
   is
      Current       : constant Point_2D :=
        Create_Point_2D
          (Kinematics.X (Position (Leg)), Kinematics.Y (Position (Leg)));
      Workspace     : constant Circle_2D :=
        Standard.Legs.Workspace.Get_Bounded_Circle (Leg);
      Shape         : constant Circle_2D :=
        Standard.Legs.Workspace.Get_Workspace_Shape (Leg);
      Ticks         : Natural;
      MTicks        : Natural;

   begin
      --  Special case to shutdown on stop

      if Velocity (Velocity_Bank).RVX = 0.0
        and Velocity (Velocity_Bank).RVY = 0.0
        and Velocity (Velocity_Bank).RVW = 0.0
      then
         Standard.Legs.Trajectory_Generator.Set_Stance (Leg);

         if Is_Identical (Current, Center (Workspace)) then
            State (Leg) :=
              (Kind      => Stance,
               PEP_Tick  => Natural'Last,
               MPEP_Tick => Natural'Last);

         else
            State (Leg) :=
              (Kind      => Stance,
               PEP_Tick  => Current_Tick,
               MPEP_Tick => Natural'Last);
         end if;

         Debug.Log.Put
           (" (S) "
            & Leg_Index'Image (Leg)
            & Integer'Image (State (Leg).PEP_Tick)
            & Integer'Image (State (Leg).MPEP_Tick));

         return;
      end if;

      Ticks :=
        Trajectory.Remaining_Ticks
          (Velocity (Velocity_Bank).Trajectory, Workspace, Current);
      MTicks :=
        Trajectory.Remaining_Ticks
          (Velocity (Velocity_Bank).Trajectory, Shape, Current);

      Standard.Legs.Trajectory_Generator.Set_Stance (Leg);

      if Ticks = 0 then
         State (Leg) :=
           (Kind     => Stance,
            PEP_Tick =>
              (if State (Leg).Kind = Stance
                 then State (Leg).PEP_Tick
               else Current_Tick),
            MPEP_Tick =>
              (if MTicks = Natural'Last
                 then Natural'Last
                 else Current_Tick + MTicks));

      else
         State (Leg) :=
           (Kind     => Stance,
            PEP_Tick =>
              (if Ticks = Natural'Last
                 then Natural'Last
                 else Current_Tick + Ticks),
            MPEP_Tick =>
              (if MTicks = Natural'Last
                 then Natural'Last
                 else Current_Tick + MTicks));
      end if;

      Debug.Log.Put
        (" "
         & Leg_Index'Image (Leg)
         & Integer'Image (State (Leg).PEP_Tick)
         & Integer'Image (State (Leg).MPEP_Tick));
   end Compute_Linear;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Current_Tick                 := 0;

      Velocity_Bank                := False;
      Velocity (Velocity_Bank).RVX := 0.0;
      Velocity (Velocity_Bank).RVY := 0.0;
      Velocity_Changed             := False;

      for Leg in Leg_Index loop
         State (Leg) :=
           (Kind      => Stance,
            PEP_Tick  => Natural'Last,
            MPEP_Tick => Natural'Last);
      end loop;

      Trajectory_Generator.Trajectory :=
        Velocity (Velocity_Bank).Trajectory'Access;
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

      if (State (Prev).PEP_Tick > State (Leg).PEP_Tick
          and State (Leg).PEP_Tick = State (Next).PEP_Tick)
        or (State (Prev).PEP_Tick = State (Leg).PEP_Tick
            and State (Leg).PEP_Tick < State (Next).PEP_Tick)
        or (State (Prev).PEP_Tick = State (Leg).PEP_Tick
            and State (Leg).PEP_Tick = State (Next).PEP_Tick)
      then
         --  Leg and its neighboring legs are equal to PEP, select one of the
         --  leg.

         return Leg_Index'Pos (Leg) mod 2 = 1;
      end if;

      return False;
   end Is_Ahead;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Point_1 : Point_2D; Point_2 : Point_2D) return Boolean
   is
      use CGK.Primitives.XYs;

      XY_1 : constant CGK.Primitives.XYs.XY :=
        CGK.Primitives.Points_2D.XY (Point_1);
      XY_2 : constant CGK.Primitives.XYs.XY :=
        CGK.Primitives.Points_2D.XY (Point_2);

   begin
      return Modulus (XY_1 - XY_2) < 0.001;
   end Is_Identical;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Point : Point_2D; Position : Kinematics.Position) return Boolean
   is
      use CGK.Primitives.XYs;

      Point_XY    : constant CGK.Primitives.XYs.XY :=
        CGK.Primitives.Points_2D.XY (Point);
      Position_XY : constant CGK.Primitives.XYs.XY :=
        Create_XY (Kinematics.X (Position), Kinematics.Y (Position));

   begin
      return Modulus (Point_XY - Position_XY) < 0.001;
   end Is_Identical;

   ------------------
   -- Set_Velocity --
   ------------------

   procedure Set_Velocity
     (RVX : CGK.Reals.Real;
      RVY : CGK.Reals.Real;
      RVW : CGK.Reals.Real) is
   begin
      if RVX /= Velocity (Velocity_Bank).RVX
        or RVY /= Velocity (Velocity_Bank).RVY
        or RVW /= Velocity (Velocity_Bank).RVW
      then
         Velocity (not Velocity_Bank).RVX := RVX;
         Velocity (not Velocity_Bank).RVY := RVY;
         Velocity (not Velocity_Bank).RVW := RVW;

         Standard.Legs.Trajectory.Set_Relative_Velocity
           (Self       => Velocity (not Velocity_Bank).Trajectory,
            Velocity_X => RVX,
            Velocity_Y => RVY,
            Velocity_W => RVW);

         Velocity_Changed := True;
      end if;
   end Set_Velocity;

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

   ----------
   -- Tick --
   ----------

   procedure Tick is
   begin
      --  Update state of the landed legs.

      declare
         New_Line : Boolean := False;

      begin
         for Leg in Leg_Index loop
            if State (Leg).Kind = Swing
              and then State (Leg).AEP_Tick <= Current_Tick
            then
               Compute_Linear (Leg);
               New_Line := True;
            end if;
         end loop;

         if New_Line then
            Debug.Log.Put (" (touchdown)");
            Debug.Log.New_Line;
         end if;
      end;

      if Velocity_Changed then
         --  Velocity has been changed, recompute path of legs in stance.

         Velocity_Bank := not Velocity_Bank;

         declare
            New_Line : Boolean := False;

         begin
            for Leg in Leg_Index loop
               if State (Leg).Kind = Stance then
                  Compute_Linear (Leg);
                  New_Line := True;
               end if;
            end loop;

            if New_Line then
               Debug.Log.New_Line;
            end if;
         end;
         Trajectory_Generator.Trajectory :=
           Velocity (Velocity_Bank).Trajectory'Access;

         Velocity_Changed := False;
      end if;

      for Leg in Leg_Index'Range loop
         if Is_Ahead (Leg) then
            if State (Leg).Kind = Stance then
               if Start_Swing (Leg) then
                  declare
                     AEP : constant Point_2D :=
                       Trajectory.Anterior_Extreme_Position
                         (Velocity (Velocity_Bank).Trajectory, Leg);

                  begin
                     if Is_Identical (AEP, Position (Leg)) then
                        --  Don't switch to swing when position is close to AEP

                        Compute_Linear (Leg);

                     else
                        Standard.Legs.Trajectory_Generator.Set_Swing
                          (Leg    => Leg,
                           AEP_X  => X (AEP),
                           AEP_Y  => Y (AEP),
                           Height => 0.030);
                        State (Leg) :=
                          (Kind     => Swing,
                           AEP_Tick => Current_Tick + Swing_Ticks);
                     end if;
                  end;
               end if;

            else
               if State (Leg).AEP_Tick <= Current_Tick then
                  raise Program_Error;
               end if;
            end if;
         end if;
      end loop;

      declare
         Legs     : array (Leg_Index) of Boolean := [others => False];
         Deadlock : Boolean := False;

      begin
         for Leg in Leg_Index'Range loop
            if State (Leg).Kind = Stance
              and then State (Leg).MPEP_Tick < Current_Tick + Swing_Ticks
            then
               Legs (Leg) := True;
               Deadlock := True;
            end if;
         end loop;

         if Deadlock then
            Debug.Log.Put_Line
              ("Deadlock"
                 & (if Legs (Left_Front)   then " LF" else "")
                 & (if Legs (Left_Middle)  then " LM" else "")
                 & (if Legs (Left_Hind)    then " LH" else "")
                 & (if Legs (Right_Front)  then " RF" else "")
                 & (if Legs (Right_Middle) then " RM" else "")
                 & (if Legs (Right_Hind)   then " RH" else "")
              );
         end if;
      end;

      Current_Tick := @ + 1;
   end Tick;

end Legs.Gait_Generator;
