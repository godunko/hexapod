--
--  Copyright (C) 2023-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Command_Line;
with Ada.Text_IO;

with Kinematics;
with Reals;
with Trajectory.Steps.Planner;

procedure Trajectory_Steps is

   use Ada.Text_IO;

   package Real_IO is new Ada.Text_IO.Float_IO (Reals.Real);
   use Real_IO;

   LF_Base : Kinematics.Position;
   LM_Base : Kinematics.Position;
   LH_Base : Kinematics.Position;
   RF_Base : Kinematics.Position;
   RM_Base : Kinematics.Position;
   RH_Base : Kinematics.Position;

   Iterations : Positive := 10;

   procedure Do_Tick is

      use all type Trajectory.Steps.Stage_Kind;

      Duty_Factor : Reals.Real;
      Step_Plan   : Trajectory.Steps.Step_Plan_Descriptor;

      procedure Put (Item : Trajectory.Steps.Leg_Step_Plan_Descriptor) is
      begin
         if Item.Stage = Stance then
            Put (" _ ");

         else
            Put (" ^ ");
         end if;

         Put (Item.Start_Position, Fore => 2, Aft => 1, Exp => 0);
         Put (" ");
         Put (Item.End_Position, Fore => 2, Aft => 1, Exp => 0);

         --  if Item.Support then
         --     Put ("            ");

         --  else
         --     Put (" ");
         --     Put (Item.Swing_Begin, Fore => 2, Aft => 2, Exp => 0);
         --     Put (" ");
         --     Put (Item.Swing_End, Fore => 2, Aft => 2, Exp => 0);
         --  end if;
         --  case Item.Support is
         --     when False =>
         --        Put (Item.Stance_Start_Fase, Fore => 1, Aft => 3, Exp => 0);
         --
         --     when True =>
         --        Put ("     ");
         --  end case;
      end Put;

   begin
      Trajectory.Steps.Planner.Compute_Step
        (Length_X => 1.0,
         Length_Y => 1.0,
         Height_Z => 1.0,
         Result   => Step_Plan);

      Put (Step_Plan.Ratio, Fore => 1, Aft => 3, Exp => 0);
      Put (" ");
      Put (Step_Plan.LF);
      Put (" ");
      Put (Step_Plan.LM);
      Put (" ");
      Put (Step_Plan.LH);
      Put (" ");
      Put (Step_Plan.RF);
      Put (" ");
      Put (Step_Plan.RM);
      Put (" ");
      Put (Step_Plan.RH);

      New_Line;
   end Do_Tick;

begin
   if Ada.Command_Line.Argument_Count = 1 then
      Iterations := Integer'Value (Ada.Command_Line.Argument (1));
   end if;

   Kinematics.Set (LF_Base, 0.0, 0.0, 0.0);
   Kinematics.Set (LM_Base, 0.0, 0.0, 0.0);
   Kinematics.Set (LH_Base, 0.0, 0.0, 0.0);
   Kinematics.Set (RF_Base, 0.0, 0.0, 0.0);
   Kinematics.Set (RM_Base, 0.0, 0.0, 0.0);
   Kinematics.Set (RH_Base, 0.0, 0.0, 0.0);

   for J in 1 .. Iterations loop
      Do_Tick;
   end loop;

   Put_Line ("---------- WAVE ----------");
   Trajectory.Steps.Planner.Transition (Trajectory.Steps.Planner.Wave_Gait);

   for J in 1 .. Iterations loop
      Do_Tick;
   end loop;

   Put_Line ("---------- QUAD ----------");
   Trajectory.Steps.Planner.Transition (Trajectory.Steps.Planner.Quadro_Gait);

   for J in 1 .. Iterations loop
      Do_Tick;
   end loop;

   Put_Line ("---------- TRIP ----------");
   Trajectory.Steps.Planner.Transition (Trajectory.Steps.Planner.Tripod_Gait);

   for J in 1 .. Iterations loop
      Do_Tick;
   end loop;

   Put_Line ("---------- WAVE ----------");
   Trajectory.Steps.Planner.Transition (Trajectory.Steps.Planner.Wave_Gait);

   for J in 1 .. Iterations loop
      Do_Tick;
   end loop;

   Put_Line ("---------- STOP ----------");
   Trajectory.Steps.Planner.Transition (Trajectory.Steps.Planner.Stop_Gait);

   for J in 1 .. Iterations loop
      Do_Tick;
   end loop;
end Trajectory_Steps;
