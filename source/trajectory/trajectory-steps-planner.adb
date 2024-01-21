--
--  Copyright (C) 2023-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Trajectory.Steps.Planner is

   type Mask_Type is record
      L_1 : Boolean;
      L_2 : Boolean;
      L_3 : Boolean;
      L_4 : Boolean;
      L_5 : Boolean;
      L_6 : Boolean;
   end record;

   Empty    : constant Mask_Type := (others => False);
   Subset_1 : constant Mask_Type := (L_1 | L_4 | L_5 => True, others => False);
   Subset_2 : constant Mask_Type := (L_2 | L_3 | L_6 => True, others => False);

   function Is_In
     (State : Mask_Type;
      Set   : Mask_Type) return Boolean;

   function Is_In_Subset_1 (Item : Mask_Type) return Boolean;

   function Is_In_Subset_2 (Item : Mask_Type) return Boolean;

   Active_Gait            : Gait_Descriptor := Stop_Gait_Data'Access;
   Previous_Configuration : Configuration   := (0, 0, 0, 0, 0, 0);
   Current_Configuration  : Configuration   := (0, 0, 0, 0, 0, 0);
   Current_Step           : Gait_Step_Index := 0;
   Current_Step_Ticks     : Natural         := 0;
   Next_Step_Ticks        : Natural         := 0;

   procedure Compute
     (Current_Configuration : in out Configuration;
      Gait                  : Gait_Descriptor;
      Current_Step          : in out Gait_Step_Index;
      Ticks                 : in out Natural;
      Success               : in out Boolean);
   --  Compute next step.

   procedure Compute_Step
     (Current         : in out Configuration;
      Next_Gait_State : Configuration;
      Force           : Mask_Type;
      Ticks           : Natural);

   function Must_Swing
     (Item : Configuration; Ticks : Integer) return Mask_Type;
   --  Legs that must go to the swing state before the end of the step of Ticks
   --  length.

   function Swing_Stance
     (Previous : Configuration;
      Current  : Configuration;
      Ticks    : Integer) return Mask_Type;
   --  Legs that must use swing to transition from Previous to Current
   --  configuration when step has Ticks length.

   procedure Analyze_Transition
     (Known_State      : Configuration;
      Gait             : Gait_Descriptor;
      First_Step_Ticks : Natural;
      Current_Step     : out Gait_Step_Index;
      Success          : out Boolean);
   --  Checks whether transition from the given configuration to the new gait
   --  is possible and sets synchronized gait's step.

   function Is_Stable (Swing : Mask_Type) return Boolean;
   --  Checks whether given set of swing legs is statically stable
   --  configuration.

   ------------------------
   -- Analyze_Transition --
   ------------------------

   procedure Analyze_Transition
     (Known_State      : Configuration;
      Gait             : Gait_Descriptor;
      First_Step_Ticks : Natural;
      Current_Step     : out Gait_Step_Index;
      Success          : out Boolean)
   is
      Known_State_Swing : constant Mask_Type :=
        Must_Swing (Known_State, First_Step_Ticks);

   begin
      Success := False;

         --  Put (Known_State);
         --  Put (" (");
         --  Put (Known_State_Swing);
         --  Put (")");
         --
         --  --  if Is_Subset_1 (Known_State_Swing)
         --  --    and Is_Subset_2 (Known_State_Swing)
      if not Is_Stable (Known_State_Swing) then
         --  Position at First_Step_Ticks offset is not reachable, returns.

         --  --  then
         --     Success := False;
         --     Put (" UNSTABLE");
         --     New_Line;
         --
         return;
      end if;

         --  New_Line;

      for J in Gait.Step'Range loop
         --  raise Program_Error;
         --     Put ("[");
         --     Put (New_Gait.Name);
         --     Put (J, 1);
         --     Put ("] ");
         --     Put (New_Gait.State (J));

         declare
            Current : Configuration   := Known_State;
            Step    : Gait_Step_Index := J;
            Ticks   : Natural         := First_Step_Ticks;

         begin
            for K in 1 .. 3 loop
               declare
                  Current_State_Swing : constant Mask_Type :=
                    Must_Swing (Current, Ticks);
         --              Step_Swing          : constant Mask_Type :=
         --                Must_Swing (New_Gait.State (Step), New_Gait.Ticks);
         --              Next_Step           : constant Gait_Step_Index :=
         --                (Step + 1) mod New_Gait.State'Length;
         --              --  Next_Syep_Swing     : constant Mask_Type :=
         --              --    Must_Swing (
               begin
         --              Put (" | ");
         --              Put (Current_State_Swing);
         --              Put ("/");
         --              Put (Step_Swing);
         --
                  if Current = Gait.Step (Step) then
                     Current_Step := J;
                     Success      := True;

                     return;

                  elsif not Is_Stable (Current_State_Swing) then
                     Success := False;
         --                 Put ("  UNSTABLE");
         --
                     --  raise Program_Error;
                     exit;
         --
                  else
                     Compute (Current, Gait, Step, Ticks, Success);
                  end if;
         --
         --              Put (" =");
         --              --  Put (New_Gait.Name);
         --              Put (Ticks, 1);
         --              Put (">");
         --              Put (Current);
         --
         --              --  Ticks := New_Gait.Ticks;
         --              --  Step  := Next_Step;
               end;
            end loop;
         end;

         --     New_Line;
      end loop;
   end Analyze_Transition;

   -------------
   -- Compute --
   -------------

   procedure Compute
     (Current_Configuration : in out Configuration;
      Gait                  : Gait_Descriptor;
      Current_Step          : in out Gait_Step_Index;
      Ticks                 : in out Natural;
      Success               : in out Boolean)
   is
      Current_State_Swing : constant Mask_Type :=
        Must_Swing (Current_Configuration, Ticks);
      Current_Step_Swing  : constant Mask_Type :=
        Must_Swing (Gait.Step (Current_Step), Ticks);
      Next_Step           : constant Gait_Step_Index :=
        (Current_Step + 1) mod Gait.Step'Length;

   begin
      pragma Assert (Success);
      pragma Assert (Is_Stable (Current_State_Swing));

      if Current_Configuration = Gait.Step (Current_Step) then
         Current_Configuration := Gait.Step (Next_Step);

      elsif Current_State_Swing = Empty then
         if Is_In_Subset_1 (Current_Step_Swing)
           and not Is_In_Subset_2 (Current_Step_Swing)
         then
            Compute_Step
              (Current_Configuration,
               Gait.Step (Next_Step),
               Subset_1,
               Ticks);

         elsif not Is_In_Subset_1 (Current_Step_Swing)
           and Is_In_Subset_2 (Current_Step_Swing)
         then
            Compute_Step
              (Current_Configuration,
               Gait.Step (Next_Step),
               Subset_2,
               Ticks);

         elsif Current_Step = Next_Step then
            --  "Stop"

--                 Put (">>> ");
--                 Put (Current_State);
--                 Put (" <<<");

            if Current_Configuration.L_1 /= 0
              or Current_Configuration.L_4 /= 0
              or Current_Configuration.L_5 /= 0
            then
               Compute_Step
                 (Current_Configuration,
                  Gait.Step (Next_Step),
                  Subset_1,
                  Ticks);

            else
               Compute_Step
                 (Current_Configuration,
                  Gait.Step (Next_Step),
                  Subset_2,
                  Ticks);
            end if;

         else
--                       --           Compute_Step
--                       --             (Current,
--                       --              New_Gait.State (Next_Step),
--                       --              Step_Swing,
--                       --              Ticks);
               raise Program_Error;
         end if;

      elsif Is_In_Subset_1 (Current_State_Swing)
        and not Is_In_Subset_2 (Current_State_Swing)
      then
         Compute_Step
           (Current_Configuration,
            Gait.Step (Next_Step),
            Subset_1,
            Ticks);

      elsif not Is_In_Subset_1 (Current_State_Swing)
        and Is_In_Subset_2 (Current_State_Swing)
      then
         Compute_Step
           (Current_Configuration,
            Gait.Step (Next_Step),
            Subset_2,
            Ticks);

      elsif Is_In (Current_Step_Swing, Current_State_Swing) then
--  --              --  elsif Count (Current_State_Swing)
--  --              --    = Count (Current_State_Swing + Gait_Step_Swing)
--  --              --  then
--              Compute_Step
--                (Current_State,
--                 Gait.State (Next_Step),
--                 Current_State_Swing,
--                 Ticks);
         raise Program_Error;

      else
         Success := False;

         return;
      end if;

      Current_Step := Next_Step;
      Ticks        := Gait.Ticks;
   end Compute;

   ------------------
   -- Compute_Step --
   ------------------

   procedure Compute_Step
     (Current         : in out Configuration;
      Next_Gait_State : Configuration;
      Force           : Mask_Type;
      Ticks           : Natural)
   is
      procedure Compute
        (Current         : in out Integer;
         Next_Gait_State : Integer;
         Force           : Boolean)
      is
      begin
         if Force then
            Current := Next_Gait_State;

         else
            Current := @ - Ticks;

            pragma Assert (Current in -5 .. 5);
         end if;
      end Compute;

   begin
      Compute (Current.L_1, Next_Gait_State.L_1, Force.L_1);
      Compute (Current.L_2, Next_Gait_State.L_2, Force.L_2);
      Compute (Current.L_3, Next_Gait_State.L_3, Force.L_3);
      Compute (Current.L_4, Next_Gait_State.L_4, Force.L_4);
      Compute (Current.L_5, Next_Gait_State.L_5, Force.L_5);
      Compute (Current.L_6, Next_Gait_State.L_6, Force.L_6);
   end Compute_Step;

   ------------------
   -- Compute_Step --
   ------------------

   procedure Compute_Step
     (Length_X : Reals.Real;
      Length_Y : Reals.Real;
      Height_Z : Reals.Real;
      Result   : out Step_Plan_Descriptor)
   is

      procedure Compute
        (Result        : out Leg_Step_Plan_Descriptor;
         Stance_Factor : Reals.Real;
         Is_Swing      : Boolean;
         Previous      : Integer;
         Current       : Integer);

      -------------
      -- Compute --
      -------------

      procedure Compute
        (Result        : out Leg_Step_Plan_Descriptor;
         Stance_Factor : Reals.Real;
         Is_Swing      : Boolean;
         Previous      : Integer;
         Current       : Integer) is
      begin
         if Is_Swing then
            Result :=
              (Stage          => Swing,
               Length_X       => Length_X,
               Length_Y       => Length_Y,
               Height_Z       => Height_Z,
               Start_Position =>
                 Reals.Real (Previous) / Reals.Real (Support_Ticks),
               End_Position   =>
                 Reals.Real (Current) / Reals.Real (Support_Ticks));

         else
            Result :=
              (Stage          => Stance,
               Length_X       => Length_X,
               Length_Y       => Length_Y,
               Start_Position =>
                 Reals.Real (Previous) / Reals.Real (Support_Ticks),
               End_Position   =>
                 Reals.Real (Current) / Reals.Real (Support_Ticks));
         end if;
      end Compute;

      Success       : Boolean := True;
      Swing         : Mask_Type;
      Stance_Factor : Reals.Real := 1.0;

   begin
      Previous_Configuration := Current_Configuration;
      Current_Step_Ticks     := Next_Step_Ticks;

      --  Compute velocity ratio.

      Result.Ratio :=
        Reals.Real (Current_Step_Ticks) / Reals.Real (Support_Ticks);

      --     Next_Step           : constant Gait_Step_Index :=
      --       (Self.Step + 1) mod Self.Gait.State'Length;
      --
      --  begin
      --     Put (Previous_State);
      --     Put (" [");
      --     Put (Self.Gait.Name);
      --     Put (Self.Step, 1);
      --     Put ("] =");
      --     Put (Self.Ticks, 1);
      --     Put (">");

      Compute
        (Current_Configuration,
         Active_Gait,
         Current_Step,
         Next_Step_Ticks,
         Success);
      pragma Assert (Success);

      Swing :=
        Swing_Stance
          (Previous_Configuration, Current_Configuration, Current_Step_Ticks);

      Compute
        (Result        => Result.LF,
         Stance_Factor => Stance_Factor,
         Is_Swing      => Swing.L_1,
         Previous      => Previous_Configuration.L_1,
         Current       => Current_Configuration.L_1);
      Compute
        (Result        => Result.RF,
         Stance_Factor => Stance_Factor,
         Is_Swing      => Swing.L_2,
         Previous      => Previous_Configuration.L_2,
         Current       => Current_Configuration.L_2);
      Compute
        (Result        => Result.LM,
         Stance_Factor => Stance_Factor,
         Is_Swing      => Swing.L_3,
         Previous      => Previous_Configuration.L_3,
         Current       => Current_Configuration.L_3);
      Compute
        (Result        => Result.RM,
         Stance_Factor => Stance_Factor,
         Is_Swing      => Swing.L_4,
         Previous      => Previous_Configuration.L_4,
         Current       => Current_Configuration.L_4);
      Compute
        (Result        => Result.LH,
         Stance_Factor => Stance_Factor,
         Is_Swing      => Swing.L_5,
         Previous      => Previous_Configuration.L_5,
         Current       => Current_Configuration.L_5);
      Compute
        (Result        => Result.RH,
         Stance_Factor => Stance_Factor,
         Is_Swing      => Swing.L_6,
         Previous      => Previous_Configuration.L_6,
         Current       => Current_Configuration.L_6);

      --  if Swing.L_1 then
      --     LF :=
      --       (Support     => False,
      --        Start_Fase  =>
      --          Stance_Factor
      --            * Reals.Real (5 - Previous_Configuration.L_1)
      --            / Reals.Real (Support_Ticks),
      --        End_Fase    =>
      --          Stance_Factor
      --            * Reals.Real (5 - Current_Configuration.L_1)
      --            / Reals.Real (Support_Ticks),
      --        Swing_Begin =>
      --          Reals.Real (Previous_Configuration.L_1)
      --            / Reals.Real (Support_Ticks),
      --        Swing_End   =>
      --          Reals.Real (Current_Configuration.L_1)
      --            / Reals.Real (Support_Ticks));
      --
      --  else
      --     LF :=
      --       (Support    => True,
      --        Start_Fase =>
      --          Stance_Factor
      --            * Reals.Real (5 - Previous_Configuration.L_1)
      --            / Reals.Real (Support_Ticks),
      --        End_Fase   =>
      --          Stance_Factor
      --            * Reals.Real (5 - Current_Configuration.L_1)
      --            / Reals.Real (Support_Ticks));
      --  end if;
      --
      --  if Swing.L_2 then
      --     RF :=
      --       (Support           => False,
      --        Stance_Start_Fase =>
      --          Stance_Factor
      --            * Reals.Real (5 - Current_Configuration.L_2) / 10.0);
      --  else
      --     RF:= (Support => True);
      --  end if;
      --
      --  if Swing.L_3 then
      --     LM :=
      --       (Support           => False,
      --        Stance_Start_Fase =>
      --          Stance_Factor
      --            * Reals.Real (5 - Current_Configuration.L_3) / 10.0);
      --  else
      --     LM:= (Support => True);
      --  end if;
      --
      --  if Swing.L_4 then
      --     RM :=
      --       (Support           => False,
      --        Stance_Start_Fase =>
      --          Stance_Factor
      --            * Reals.Real (5 - Current_Configuration.L_4) / 10.0);
      --  else
      --     RM:= (Support => True);
      --  end if;
      --
      --  if Swing.L_5 then
      --     LH :=
      --       (Support           => False,
      --        Stance_Start_Fase =>
      --          Stance_Factor
      --            * Reals.Real (5 - Current_Configuration.L_5) / 10.0);
      --  else
      --     LH:= (Support => True);
      --  end if;
      --
      --  if Swing.L_6 then
      --     RH :=
      --       (Support           => False,
      --        Stance_Start_Fase =>
      --          Stance_Factor
      --            * Reals.Real (5 - Current_Configuration.L_6) / 10.0);
      --  else
      --     RH:= (Support => True);
      --  end if;
   end Compute_Step;

   ---------------
   -- Is_Stable --
   ---------------

   function Is_Stable (Swing : Mask_Type) return Boolean is
      Table :
        array (Boolean, Boolean, Boolean, Boolean, Boolean, Boolean)
        of Boolean :=
          (others => (others => (others => (others => (others => (others => False))))));

   begin
      Table (False, True,  True,  True,  True,  False) := True;
      Table (False, True,  False, True,  False, True)  := True;
      Table (False, True,  False, True,  True,  True)  := True;
      Table (False, True,  True,  True,  False, True)  := True;
      Table (False, True,  True,  True,  True,  True)  := True;
      Table (True,  False, True,  False, True,  False) := True;
      Table (True,  False, True,  True,  True,  False) := True;
      Table (True,  False, True,  False, True,  True)  := True;
      Table (True,  False, True,  True,  False, True)  := True;
      Table (True,  False, True,  True,  True,  True)  := True;
      Table (True,  True,  True,  False, True,  False) := True;
      Table (True,  True,  True,  True,  True,  False) := True;
      Table (True,  True,  False, False, True,  True)  := True;
      Table (True,  True,  False, True,  False, True)  := True;
      Table (True,  True,  False, True,  True,  True)  := True;
      Table (True,  True,  True,  False, True,  True)  := True;
      Table (True,  True,  True,  True,  False, True)  := True;
      Table (True,  True,  True,  True,  True,  True)  := True;

      return
        Table
          (not Swing.L_1, not Swing.L_3, not Swing.L_5,
           not Swing.L_2, not Swing.L_4, not Swing.L_6);
      --  if Count (Swing) = 1 then
      --     return True;
      --
      --  elsif  then
      --  end if;
   end Is_Stable;

   -----------
   -- Is_In --
   -----------

   function Is_In
     (State : Mask_Type;
      Set   : Mask_Type) return Boolean is
   begin
      --  Put (State);
      --  Put ("/");
      --  Put (Set);

      return Result : Boolean := True do
         if State.L_1 and not Set.L_1 then
            Result := False;
         end if;

         if State.L_2 and not Set.L_2 then
            Result := False;
         end if;

         if State.L_3 and not Set.L_3 then
            Result := False;
         end if;

         if State.L_4 and not Set.L_4 then
            Result := False;
         end if;

         if State.L_5 and not Set.L_5 then
            Result := False;
         end if;

         if State.L_6 and not Set.L_6 then
            Result := False;
         end if;
      end return;
   end Is_In;

   --------------------
   -- Is_In_Subset_1 --
   --------------------

   function Is_In_Subset_1 (Item : Mask_Type) return Boolean is
   begin
      return Item.L_1 or Item.L_4 or Item.L_5;
   end Is_In_Subset_1;

   --------------------
   -- Is_In_Subset_2 --
   --------------------

   function Is_In_Subset_2 (Item : Mask_Type) return Boolean is
   begin
      return Item.L_2 or Item.L_3 or Item.L_6;
   end Is_In_Subset_2;

   ----------------
   -- Must_Swing --
   ----------------

   function Must_Swing
     (Item : Configuration; Ticks : Integer) return Mask_Type is
   begin
      return Result : Mask_Type do
         Result.L_1 := Item.L_1 - Ticks < -5;
         Result.L_2 := Item.L_2 - Ticks < -5;
         Result.L_3 := Item.L_3 - Ticks < -5;
         Result.L_4 := Item.L_4 - Ticks < -5;
         Result.L_5 := Item.L_5 - Ticks < -5;
         Result.L_6 := Item.L_6 - Ticks < -5;
      end return;
   end Must_Swing;

   ------------------
   -- Swing_Stance --
   ------------------

   function Swing_Stance
     (Previous : Configuration;
      Current  : Configuration;
      Ticks    : Integer) return Mask_Type is
   begin
      return
        (L_1 => (Current.L_1 /= Previous.L_1 - Ticks),
         L_2 => (Current.L_2 /= Previous.L_2 - Ticks),
         L_3 => (Current.L_3 /= Previous.L_3 - Ticks),
         L_4 => (Current.L_4 /= Previous.L_4 - Ticks),
         L_5 => (Current.L_5 /= Previous.L_5 - Ticks),
         L_6 => (Current.L_6 /= Previous.L_6 - Ticks));
   end Swing_Stance;

   ----------------
   -- Transition --
   ----------------

   procedure Transition (Gait : Gait_Descriptor) is
      Active_Gait_Ticks : constant Natural := Active_Gait.Ticks;
           --  (if Self.Gait = null then 0 else Self.Gait.Ticks);
      Success           : Boolean          := True;

      --     Current_State_Swing : constant Mask_Type :=
      --       Must_Swing (Self.State, 1);
      --     Previous_State      : constant State := Self.State;
      --
      --  begin
      --     Put (Previous_State);
      --     Put (" (");
      --     --  Put (Current_State_Swing);
      --     Put (")");
      --     New_Line;
      --
      --     for J in New_Gait.State'Range loop
      --        Put ("[");
      --        Put (New_Gait.Name);
      --        Put (J, 1);
      --        Put ("] ");
      --        Put (New_Gait.State (J));
      --
      --        declare
      --           Current : State := Self.State;
      --           Step    : Gait_Step_Index := J;
      --
      --        begin
      --           for K in 1 .. 6 loop
      --              null;
      --           end loop;
      --        end;
      --
      --        New_Line;
      --     end loop;
   begin
      Active_Gait     := Gait;
      Next_Step_Ticks := Gait.Ticks;

      Analyze_Transition
        (Current_Configuration, Gait, Gait.Ticks, Current_Step, Success);

      if not Success then
         Next_Step_Ticks := Active_Gait_Ticks;
         Analyze_Transition
           (Current_Configuration,
            Gait,
            Active_Gait_Ticks,
            Current_Step,
            Success);
      end if;

      if not Success then
         raise Program_Error;
      end if;
   end Transition;

end Trajectory.Steps.Planner;
