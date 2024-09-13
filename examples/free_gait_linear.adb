--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO; use Ada.Text_IO;

with Legs; use Legs;
--  with Legs.Workspace;

procedure Free_Gait_Linear is

   type Leg_State is record
      Stance    : Boolean;
      --  Whether leg is in stance

      PEP_Ticks : Natural;
      --  Number of control loop ticks till Posterior Extreme Point be reached.

      AEP_Ticks : Natural;
      --  Number of control loop ticks till Anterior Extreme Point be reached.
   end record;

   State  : array (Legs.Leg_Index) of Leg_State;
   Locked : array (Legs.Leg_Index) of Boolean;

   Swing_Ticks  : constant := 4;
   Stance_Ticks : constant := 5;

   function Is_Ahead (Leg : Legs.Leg_Index) return Boolean;

   function Start_Swing (Leg : Legs.Leg_Index) return Boolean;

   --------------
   -- Is_Ahead --
   --------------

   function Is_Ahead (Leg : Legs.Leg_Index) return Boolean is
      Prev : constant Legs.Leg_Index :=
        (if Leg = Leg_Index'First then Leg_Index'Last else Leg_Index'Pred (Leg));
      Next : constant Legs.Leg_Index :=
        (if Leg = Leg_Index'Last then Leg_Index'First else Leg_Index'Succ (Leg));

   begin
      if not State (Leg).Stance then
         return True;
      end if;

      if State (Prev).PEP_Ticks > State (Leg).PEP_Ticks
        and State (Leg).PEP_Ticks < State (Next).PEP_Ticks
      then
         return True;
      end if;

      if State (Prev).PEP_Ticks = State (Leg).PEP_Ticks
        and State (Leg).PEP_Ticks = State (Next).PEP_Ticks
      then
         return Leg_Index'Pos (Leg) mod 2 = 1;
      end if;

      return False;
   end Is_Ahead;

   -----------------
   -- Start_Swing --
   -----------------

   function Start_Swing (Leg : Legs.Leg_Index) return Boolean is
      Prev : constant Legs.Leg_Index :=
        (if Leg = Leg_Index'First then Leg_Index'Last else Leg_Index'Pred (Leg));
      Next : constant Legs.Leg_Index :=
        (if Leg = Leg_Index'Last then Leg_Index'First else Leg_Index'Succ (Leg));

   begin
      return
        State (Leg).PEP_Ticks = 0
        or else State (Prev).PEP_Ticks <= Swing_Ticks
        or else State (Next).PEP_Ticks <= Swing_Ticks;
   end Start_Swing;

begin
   State := (others => (Stance => True, PEP_Ticks => Stance_Ticks / 2, AEP_Ticks => 0));
   --  Legs.Initialize;
   --  Legs.Workspace.Compute (0.030);

   for Tick in 0 .. 20 loop
      Locked := (others => False);

      --  First, proces touch down of the legs.

      for Leg in Leg_Index'Range loop
         if not State (Leg).Stance and State (Leg).AEP_Ticks = 0
         then
            State (Leg) := (True, Stance_Ticks, 0);
            Put (Leg'Img);
            Put (" v ");
         end if;
      end loop;

      for Leg in Leg_Index'Range loop
         if Is_Ahead (Leg) then
            Put (Leg'Img);

            if State (Leg).Stance then
               if Start_Swing (Leg) then
                  State (Leg) := (False, 0, Swing_Ticks);
                  Put (" ^ ");

               else
                  Put ("   ");
               end if;

            else
               if State (Leg).AEP_Ticks = 0 then
                  State (Leg) := (True, Stance_Ticks, 0);
                  Put (" v ");

               else
                  Put ("   ");
               end if;
            end if;
         end if;
      end loop;

      for Leg in Leg_Index'Range loop
         if State (Leg).Stance then
            if State (Leg).PEP_Ticks /= 0 then
               State (Leg).PEP_Ticks := @ - 1;

            else
               Put (Leg'Img);
               Put (" S ");
            end if;

         else
            State (Leg).AEP_Ticks := @ - 1;
         end if;
      end loop;

      New_Line;
   end loop;
end Free_Gait_Linear;
