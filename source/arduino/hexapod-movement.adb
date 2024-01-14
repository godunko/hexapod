--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with Ada.Numerics;
with Interfaces;

with BBF.Board;
with BBF.PCA9685;

with Kinematics.Forward;
with Kinematics.Inverse.Geometric;
with Trajectory.Gait;

with Hexapod.Console;
--  with Hexapod.Debug;
with Hexapod.Hardware;

package body Hexapod.Movement is

   use type Reals.Real;

   type Motor_Descriptor is record
      Channel   : not null access BBF.PCA9685.PCA9685_Channel'Class;
      Min_PWM   : BBF.PCA9685.Value_Type;
      Max_PWM   : BBF.PCA9685.Value_Type;
      Min_Angle : Reals.Real;
      Max_Angle : Reals.Real;
   end record;

   LF_Base : Kinematics.Position;
   LM_Base : Kinematics.Position;
   LH_Base : Kinematics.Position;
   RF_Base : Kinematics.Position;
   RM_Base : Kinematics.Position;
   RH_Base : Kinematics.Position;

   LF_M_1 : constant Motor_Descriptor :=
     (Hexapod.Hardware.LF_Motor_1_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   LF_M_2 : constant Motor_Descriptor :=
     (Hexapod.Hardware.LF_Motor_2_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   LF_M_3 : constant Motor_Descriptor :=
     (Hexapod.Hardware.LF_Motor_3_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0 - 0.16,
      Ada.Numerics.Pi - 0.32);

   LM_M_1 : constant Motor_Descriptor :=
     (Hexapod.Hardware.LM_Motor_1_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   LM_M_2 : constant Motor_Descriptor :=
     (Hexapod.Hardware.LM_Motor_2_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   LM_M_3 : constant Motor_Descriptor :=
     (Hexapod.Hardware.LM_Motor_3_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0 - 0.16,
      Ada.Numerics.Pi - 0.32);

   LH_M_1 : constant Motor_Descriptor :=
     (Hexapod.Hardware.LH_Motor_1_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   LH_M_2 : constant Motor_Descriptor :=
     (Hexapod.Hardware.LH_Motor_2_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   LH_M_3 : constant Motor_Descriptor :=
     (Hexapod.Hardware.LH_Motor_3_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0 - 0.16,
      Ada.Numerics.Pi - 0.32);

   RF_M_1 : constant Motor_Descriptor :=
     (Hexapod.Hardware.RF_Motor_1_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   RF_M_2 : constant Motor_Descriptor :=
     (Hexapod.Hardware.RF_Motor_2_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   RF_M_3 : constant Motor_Descriptor :=
     (Hexapod.Hardware.RF_Motor_3_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi + 0.16,
      Ada.Numerics.Pi / 2.0 + 0.32);

   RM_M_1 : constant Motor_Descriptor :=
     (Hexapod.Hardware.RM_Motor_1_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   RM_M_2 : constant Motor_Descriptor :=
     (Hexapod.Hardware.RM_Motor_2_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   RM_M_3 : constant Motor_Descriptor :=
     (Hexapod.Hardware.RM_Motor_3_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi + 0.16,
      Ada.Numerics.Pi / 2.0 + 0.32);

   RH_M_1 : constant Motor_Descriptor :=
     (Hexapod.Hardware.RH_Motor_1_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   RH_M_2 : constant Motor_Descriptor :=
     (Hexapod.Hardware.RH_Motor_2_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi / 2.0,
      Ada.Numerics.Pi / 2.0);
   RH_M_3 : constant Motor_Descriptor :=
     (Hexapod.Hardware.RH_Motor_3_Channel.all'Access,
      658,
      3289,
      -Ada.Numerics.Pi + 0.16,
      Ada.Numerics.Pi / 2.0 + 0.32);

   Wave_Gait     : constant Trajectory.Gait.Gait_Descriptor :=
     (1.0 / 2.0,
      0.0,
      1.0 / 2.0,
      0.0,
      1.0 / 2.0,
      0.0,
      1.0 / 2.0);
   --  Wave_Gait     : constant Trajectory.Gait.Gait_Descriptor :=
   --    (LF_Fase     => 5.0 / 6.0,
   --     LM_Fase     => 4.0 / 6.0,
   --     LH_Fase     => 3.0 / 6.0,
   --     RH_Fase     => 2.0 / 6.0,
   --     RM_Fase     => 1.0 / 6.0,
   --     RF_Fase     => 0.0,
   --     Duty_Factor => 9.0 / 10.0);

   Tick_Duration : constant := 1.0 / Ticks;

   Cycle_Time    : Reals.Real := 0.0;

   Step_Length_X : Reals.Real := 0.000;
   Step_Length_Y : Reals.Real := 0.000;
   Step_Height   : constant := 0.020;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Posture : Kinematics.Posture;

   begin
      Kinematics.Set
        (Posture,
         Ada.Numerics.Pi / 12.0,
         Ada.Numerics.Pi / 6.0,
         -Ada.Numerics.Pi * 4.0 / 6.0);
      LF_Base := Kinematics.Forward.LF_E_Position (Posture);

      Kinematics.Set
        (Posture,
         0.0,
         Ada.Numerics.Pi / 6.0,
         -Ada.Numerics.Pi * 4.0 / 6.0);
      LM_Base := Kinematics.Forward.LM_E_Position (Posture);

      Kinematics.Set
        (Posture,
         -Ada.Numerics.Pi / 12.0,
         Ada.Numerics.Pi / 6.0,
         -Ada.Numerics.Pi * 4.0 / 6.0);
      LH_Base := Kinematics.Forward.LH_E_Position (Posture);

      Kinematics.Set
        (Posture,
         -Ada.Numerics.Pi / 12.0,
         -Ada.Numerics.Pi / 6.0,
         Ada.Numerics.Pi * 4.0 / 6.0);
      RF_Base := Kinematics.Forward.RF_E_Position (Posture);

      Kinematics.Set
        (Posture,
         0.0,
         -Ada.Numerics.Pi / 6.0,
         Ada.Numerics.Pi * 4.0 / 6.0);
      RM_Base := Kinematics.Forward.RM_E_Position (Posture);

      Kinematics.Set
        (Posture,
         Ada.Numerics.Pi / 12.0,
         -Ada.Numerics.Pi / 6.0,
         Ada.Numerics.Pi * 4.0 / 6.0);
      RH_Base := Kinematics.Forward.RH_E_Position (Posture);
   end Initialize;

   ----------
   -- Move --
   ----------

   procedure Move
     (Posture : Kinematics.Posture;
      M_1     : Motor_Descriptor;
      M_2     : Motor_Descriptor;
      M_3     : Motor_Descriptor;
      Wait    : Interfaces.Unsigned_32)
   is
      function Map
        (Descriptor : Motor_Descriptor;
         Angle      : Reals.Real) return BBF.PCA9685.Value_Type;

      ---------
      -- Map --
      ---------

      function Map
        (Descriptor : Motor_Descriptor;
         Angle      : Reals.Real) return BBF.PCA9685.Value_Type
      is
         use type BBF.PCA9685.Value_Type;

         Angle_Middle : constant Reals.Real :=
           (Descriptor.Min_Angle + Descriptor.Max_Angle) / 2.0;
         Angle_Range  : constant Reals.Real :=
           Descriptor.Max_Angle - Descriptor.Min_Angle;
         PWM_Range    : constant Reals.Real :=
           Reals.Real (Descriptor.Max_PWM - Descriptor.Min_PWM);

      begin
         return
           BBF.PCA9685.Value_Type
             ((Angle - Descriptor.Min_Angle)
              / ( Descriptor.Max_Angle - Descriptor.Min_Angle) * PWM_Range
                 + Reals.Real (Descriptor.Min_PWM));
      end Map;

      A : BBF.PCA9685.Value_Type;
      B : BBF.PCA9685.Value_Type;
      C : BBF.PCA9685.Value_Type;

   begin
      --  Console.Put_Line (Debug.Posture_Image (Posture));

      A := Map (M_1, - Kinematics.Theta_1 (Posture));
      B := Map (M_2, + Kinematics.Theta_2 (Posture));
      C := Map (M_3, - Kinematics.Theta_3 (Posture));
      --  Motors of the first and third joints not installed on the base,
      --  they rotate themself. Thus, '-' is necessary to "revert" rotation,
      --  or mathematical model need to be fixed.

      --  Console.Put_Line
      --    ("Motors : "
      --     & BBF.PCA9685.Value_Type'Image (A)
      --     & BBF.PCA9685.Value_Type'Image (B)
      --     & BBF.PCA9685.Value_Type'Image (C));

      M_3.Channel.Set (0, C);
      BBF.Board.Delay_Controller.Delay_Milliseconds (Wait);
      M_1.Channel.Set (0, A);
      BBF.Board.Delay_Controller.Delay_Milliseconds (Wait);
      M_2.Channel.Set (0, B);
      BBF.Board.Delay_Controller.Delay_Milliseconds (Wait);
   end Move;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
      LF_Position : Kinematics.Position;
      LM_Position : Kinematics.Position;
      LH_Position : Kinematics.Position;
      RF_Position : Kinematics.Position;
      RM_Position : Kinematics.Position;
      RH_Position : Kinematics.Position;
      Posture     : Kinematics.Posture;
      Success     : Boolean;

   begin
      Trajectory.Gait.Position
        (Descriptor  => Wave_Gait,
         LF_Base     => LF_Base,
         LM_Base     => LM_Base,
         LH_Base     => LH_Base,
         RF_Base     => RF_Base,
         RM_Base     => RM_Base,
         RH_Base     => RH_Base,
         Cycle       => Cycle,
         Time        => Cycle_Time,
         Length_X    => Step_Length_X,
         Length_Y    => Step_Length_Y,
         Height_Z    => Step_Height,
         LF_Position => LF_Position,
         LM_Position => LM_Position,
         LH_Position => LH_Position,
         RF_Position => RF_Position,
         RM_Position => RM_Position,
         RH_Position => RH_Position);

      --  LF & RH

      Kinematics.Inverse.Geometric.LF_Solve (LF_Position, Posture, Success);

      if Success then
         Move (Posture, LF_M_1, LF_M_2, LF_M_3, 50);

      else
         Console.Put (" NO SOLUTION");
      end if;

      Kinematics.Inverse.Geometric.RH_Solve (RH_Position, Posture, Success);

      if Success then
         Move (Posture, RH_M_1, RH_M_2, RH_M_3, 50);

      else
         Console.Put (" NO SOLUTION");
      end if;

      --  LM & RM

      Kinematics.Inverse.Geometric.LM_Solve (LM_Position, Posture, Success);

      if Success then
         Move (Posture, LM_M_1, LM_M_2, LM_M_3, 50);

      else
         Console.Put (" NO SOLUTION");
      end if;

      Kinematics.Inverse.Geometric.RM_Solve (RM_Position, Posture, Success);

      if Success then
         Move (Posture, RM_M_1, RM_M_2, RM_M_3, 50);

      else
         Console.Put (" NO SOLUTION");
      end if;

      --  LH & RF

      Kinematics.Inverse.Geometric.LH_Solve (LH_Position, Posture, Success);

      if Success then
         Move (Posture, LH_M_1, LH_M_2, LH_M_3, 50);

      else
         Console.Put (" NO SOLUTION");
      end if;

      Kinematics.Inverse.Geometric.RF_Solve (RF_Position, Posture, Success);

      if Success then
         Move (Posture, RF_M_1, RF_M_2, RF_M_3, 50);

      else
         Console.Put (" NO SOLUTION");
      end if;
   end Prepare;

   ---------------------
   -- Set_Step_Length --
   ---------------------

   procedure Set_Step_Length
     (Step_Length_X : Reals.Real;
      Step_Length_Y : Reals.Real) is
   begin
      Movement.Step_Length_X := Step_Length_X;
      Movement.Step_Length_Y := Step_Length_Y;
   end Set_Step_Length;

   ----------
   -- Step --
   ----------

   procedure Step is
      LF_Position : Kinematics.Position;
      LM_Position : Kinematics.Position;
      LH_Position : Kinematics.Position;
      RF_Position : Kinematics.Position;
      RM_Position : Kinematics.Position;
      RH_Position : Kinematics.Position;
      Posture     : Kinematics.Posture;
      Success     : Boolean;

   begin
      Cycle_Time := @ + Tick_Duration;

      if Cycle_Time >= Cycle then
         Cycle_Time := @ - Cycle;
      end if;

      Trajectory.Gait.Position
        (Descriptor  => Wave_Gait,
         LF_Base     => LF_Base,
         LM_Base     => LM_Base,
         LH_Base     => LH_Base,
         RF_Base     => RF_Base,
         RM_Base     => RM_Base,
         RH_Base     => RH_Base,
         Cycle       => Cycle,
         Time        => Cycle_Time,
         Length_X    => Step_Length_X,
         Length_Y    => Step_Length_Y,
         Height_Z    => Step_Height,
         LF_Position => LF_Position,
         LM_Position => LM_Position,
         LH_Position => LH_Position,
         RF_Position => RF_Position,
         RM_Position => RM_Position,
         RH_Position => RH_Position);

      --  LF

      Kinematics.Inverse.Geometric.LF_Solve (LF_Position, Posture, Success);

      if Success then
         Move (Posture, LF_M_1, LF_M_2, LF_M_3, 0);

      --  else
      --     Console.Put_Line ("NO SOLUTION");
      end if;

      --  LM

      Kinematics.Inverse.Geometric.LM_Solve (LM_Position, Posture, Success);

      if Success then
         Move (Posture, LM_M_1, LM_M_2, LM_M_3, 0);

      --  else
      --     Console.Put_Line ("NO SOLUTION");
      end if;

      --  LH

      Kinematics.Inverse.Geometric.LH_Solve (LH_Position, Posture, Success);

      if Success then
         Move (Posture, LH_M_1, LH_M_2, LH_M_3, 0);

      --  else
      --     Console.Put_Line ("NO SOLUTION");
      end if;

      --  RF

      Kinematics.Inverse.Geometric.RF_Solve (RF_Position, Posture, Success);

      if Success then
         Move (Posture, RF_M_1, RF_M_2, RF_M_3, 0);

      --  else
      --     Console.Put_Line ("NO SOLUTION");
      end if;

      --  RM

      Kinematics.Inverse.Geometric.RM_Solve (RM_Position, Posture, Success);

      if Success then
         Move (Posture, RM_M_1, RM_M_2, RM_M_3, 0);

      --  else
      --     Console.Put_Line ("NO SOLUTION");
      end if;

      --  RH

      Kinematics.Inverse.Geometric.RH_Solve (RH_Position, Posture, Success);

      if Success then
         Move (Posture, RH_M_1, RH_M_2, RH_M_3, 0);

      --  else
      --     Console.Put_Line ("NO SOLUTION");
      end if;
   end Step;

end Hexapod.Movement;
