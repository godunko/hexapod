--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;

with Kinematics;
with Reals;
with Trajectory.Gait;

procedure Trajectory_Wave_Gait is

   use Ada.Text_IO;
   use type Reals.Real;

   package Real_IO is new Ada.Text_IO.Float_IO (Reals.Real);
   use Real_IO;

   Wave_Gait   : Trajectory.Gait.Gait_Descriptor :=
     (5.0 / 6.0,
      4.0 / 6.0,
      3.0 / 6.0,
      2.0 / 6.0,
      1.0 / 6.0,
      0.0,
      9.0 / 10.0);

   Steps       : constant := 60;

   Step_X      : constant := 1.0;
   Step_Y      : constant := 1.0;
   Step_Height : constant := 1.0;

   LF_Base     : Kinematics.Position;
   LM_Base     : Kinematics.Position;
   LH_Base     : Kinematics.Position;
   RF_Base     : Kinematics.Position;
   RM_Base     : Kinematics.Position;
   RH_Base     : Kinematics.Position;

   LF_Position : Kinematics.Position;
   LM_Position : Kinematics.Position;
   LH_Position : Kinematics.Position;
   RF_Position : Kinematics.Position;
   RM_Position : Kinematics.Position;
   RH_Position : Kinematics.Position;

   T           : Reals.Real;

begin
   Kinematics.Set (LF_Base, 0.0, 0.0, 0.0);
   Kinematics.Set (LM_Base, 0.0, 0.0, 0.0);
   Kinematics.Set (LH_Base, 0.0, 0.0, 0.0);
   Kinematics.Set (RF_Base, 0.0, 0.0, 0.0);
   Kinematics.Set (RM_Base, 0.0, 0.0, 0.0);
   Kinematics.Set (RH_Base, 0.0, 0.0, 0.0);



   for J in 0 .. Steps loop
      T := Reals.Real (J) / Reals.Real (Steps);
      Trajectory.Gait.Position
        (Descriptor  => Wave_Gait,
         LF_Base     => LF_Base,
         LM_Base     => LM_Base,
         LH_Base     => LH_Base,
         RF_Base     => RF_Base,
         RM_Base     => RM_Base,
         RH_Base     => RH_Base,
         Cycle      => 1.0,
         Time        => T,
         Length_X    => Step_X,
         Length_Y    => Step_Y,
         Height_Z    => Step_Height,
         LF_Position => LF_Position,
         LM_Position => LM_Position,
         LH_Position => LH_Position,
         RF_Position => RF_Position,
         RM_Position => RM_Position,
         RH_Position => RH_Position);

      Put (T, Aft => 3, Exp => 0);
      Put (" => ");
      Put (Kinematics.X (LF_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Y (LF_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Z (LF_Position), Exp => 0);

      Put (" | ");
      Put (Kinematics.X (LM_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Y (LM_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Z (LM_Position), Exp => 0);

      Put (" | ");
      Put (Kinematics.X (LH_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Y (LH_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Z (LH_Position), Exp => 0);

      Put (" | ");
      Put (Kinematics.X (RF_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Y (RF_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Z (RF_Position), Exp => 0);

      Put (" | ");
      Put (Kinematics.X (RM_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Y (RM_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Z (RM_Position), Exp => 0);

      Put (" | ");
      Put (Kinematics.X (RH_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Y (RH_Position), Exp => 0);
      Put (" ");
      Put (Kinematics.Z (RH_Position), Exp => 0);

      New_Line;
   end loop;
end Trajectory_Wave_Gait;
