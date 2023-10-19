--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with Ada.Numerics;
with Interfaces;

with BBF.Board;
with BBF.Drivers.PCA9685;

with Kinematics.Forward;
with Kinematics.Inverse.Geometric;
with Trajectory.Gait;

with Hexapod.Hardware;

package body Hexapod.Movement is

   use type Reals.Real;

   type Motor_Descriptor is record
      Min_PWM   : BBF.Drivers.PCA9685.Value_Type;
      Max_PWM   : BBF.Drivers.PCA9685.Value_Type;
      Min_Angle : Reals.Real;
      Max_Angle : Reals.Real;
   end record;

   LF_Base : Kinematics.Position;
   LM_Base : Kinematics.Position;
   LH_Base : Kinematics.Position;
   RF_Base : Kinematics.Position;
   RM_Base : Kinematics.Position;
   RH_Base : Kinematics.Position;

   M_1 : constant Motor_Descriptor :=
     (658, 3289, -Ada.Numerics.Pi / 2.0, Ada.Numerics.Pi / 2.0);
   M_2 : constant Motor_Descriptor :=
     (658, 3289, -Ada.Numerics.Pi / 2.0, Ada.Numerics.Pi / 2.0);
   M_3 : constant Motor_Descriptor :=
     (658,
      3289,
      -Ada.Numerics.Pi + 0.16, Ada.Numerics.Pi / 2.0 + 0.32);

   Wave_Gait     : constant Trajectory.Gait.Gait_Descriptor :=
     (1.0 / 2.0,
      0.0,
      0.0,
      1.0 / 2.0,
      1.0 / 2.0,
      0.0,
      1.0 / 2.0);
   --  Wave_Gait     : constant Trajectory.Gait.Gait_Descriptor :=
   --    (5.0 / 6.0,
   --     4.0 / 6.0,
   --     3.0 / 6.0,
   --     2.0 / 6.0,
   --     1.0 / 6.0,
   --     0.0,
   --     9.0 / 10.0);

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
         -Ada.Numerics.Pi / 12.0,
         -Ada.Numerics.Pi / 6.0,
         Ada.Numerics.Pi * 4.0 / 6.0);
      RF_Base := Kinematics.Forward.RF_E_Position (Posture);
   end Initialize;

   ----------
   -- Move --
   ----------

   procedure Move
     (Posture : Kinematics.Posture;
      Wait    : Interfaces.Unsigned_32)
   is
      function Map
        (Descriptor : Motor_Descriptor;
         Angle      : Reals.Real) return BBF.Drivers.PCA9685.Value_Type;

      ---------
      -- Map --
      ---------

      function Map
        (Descriptor : Motor_Descriptor;
         Angle      : Reals.Real) return BBF.Drivers.PCA9685.Value_Type
      is
         use type BBF.Drivers.PCA9685.Value_Type;

         Angle_Middle : constant Reals.Real :=
           (Descriptor.Min_Angle + Descriptor.Max_Angle) / 2.0;
         Angle_Range  : constant Reals.Real :=
           Descriptor.Max_Angle - Descriptor.Min_Angle;
         PWM_Range    : constant Reals.Real :=
           Reals.Real (Descriptor.Max_PWM - Descriptor.Min_PWM);

      begin
         return
           BBF.Drivers.PCA9685.Value_Type
             ((Angle - Descriptor.Min_Angle)
              / ( Descriptor.Max_Angle - Descriptor.Min_Angle) * PWM_Range
                 + Reals.Real (Descriptor.Min_PWM));
      end Map;

      A : BBF.Drivers.PCA9685.Value_Type;
      B : BBF.Drivers.PCA9685.Value_Type;
      C : BBF.Drivers.PCA9685.Value_Type;

   begin
      A := Map (M_1, - Kinematics.Theta_1 (Posture));
      B := Map (M_2, + Kinematics.Theta_2 (Posture));
      C := Map (M_3, - Kinematics.Theta_3 (Posture));
      --  Motors of the first and third joints not installed on the base,
      --  they rotate themself. Thus, '-' is necessary to "revert" rotation,
      --  or mathematical model need to be fixed.

      --  Console.Put_Line
      --    ("Motors : "
      --     & BBF.Drivers.PCA9685.Value_Type'Image (A)
      --     & BBF.Drivers.PCA9685.Value_Type'Image (B)
      --     & BBF.Drivers.PCA9685.Value_Type'Image (C));

      Hexapod.Hardware.Servo_Controller_Left.Set_Something (2, C);
      BBF.Board.Delay_Controller.Delay_Milliseconds (Wait);
      Hexapod.Hardware.Servo_Controller_Left.Set_Something (0, A);
      BBF.Board.Delay_Controller.Delay_Milliseconds (Wait);
      Hexapod.Hardware.Servo_Controller_Left.Set_Something (1, B);
      BBF.Board.Delay_Controller.Delay_Milliseconds (Wait);
   end Move;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
      Posture : Kinematics.Posture;
      Success : Boolean;

   begin
      Kinematics.Inverse.Geometric.RF_Solve (RF_Base, Posture, Success);

      if Success then
         Move (Posture, 500);

      --  else
      --     Console.Put (" NO SOLUTION");
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

      Kinematics.Inverse.Geometric.RF_Solve (RF_Position, Posture, Success);

      if Success then
         Move (Posture, 0);

      --  else
      --     Console.Put_Line ("NO SOLUTION");
      end if;
   end Step;

end Hexapod.Movement;
