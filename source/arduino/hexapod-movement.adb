--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  pragma Restrictions (No_Elaboration_Code);

with Ada.Numerics;
with Interfaces;

with A0B.Tasking;
with A0B.Time.Clock;

with BBF.Board;
with BBF.Delays;
with BBF.PCA9685;

with Kinematics.Forward;
with Kinematics.Inverse.Geometric;
with Trajectory.Steps.Executor;
with Trajectory.Steps.Planner;

with Hexapod.Console;
with Hexapod.Debug;
with Hexapod.Hardware;

package body Hexapod.Movement is

   use type Reals.Real;

   TCB : aliased A0B.Tasking.Task_Control_Block;

   procedure Task_Subprogram;
   --  Task thread subprogram.

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

   Tick_Duration : constant := 1.0 / Ticks;

   Cycle_Time    : Reals.Real := 0.0;

   Step_Length_X : Reals.Real := 0.000;
   Step_Length_Y : Reals.Real := 0.000;
   Step_Height   : constant := 0.020;

   Step_Plan : Trajectory.Steps.Step_Plan_Descriptor := (others => <>);
   --    (Ratio => 0.0,
   --     LF    => (Trajectory.Steps.Stance, 0.0, 0.0, 0.0, 0.0),
   --     LM    => (Trajectory.Steps.Stance, 0.0, 0.0, 0.0, 0.0),
   --     LH    => (Trajectory.Steps.Stance, 0.0, 0.0, 0.0, 0.0),
   --     RF    => (Trajectory.Steps.Stance, 0.0, 0.0, 0.0, 0.0),
   --     RM    => (Trajectory.Steps.Stance, 0.0, 0.0, 0.0, 0.0),
   --     RH    => (Trajectory.Steps.Stance, 0.0, 0.0, 0.0, 0.0));

   -----------
   -- Image --
   -----------

   function Image (Item : Trajectory.Steps.Leg_Step_Plan_Descriptor) return String is
      use type Trajectory.Steps.Stage_Kind;

   begin
      return
        (if Item.Stage = Trajectory.Steps.Stance then "  _ " else "  ^ ")
           & Hexapod.Debug.Parametric_Image (Item.Start_Position, 1)
           & Hexapod.Debug.Parametric_Image (Item.End_Position, 1);
   end Image;

   --  function Image (Item : Trajectory.Steps.Step_Plan_Descriptor) return String is
   procedure Put (Item : Trajectory.Steps.Step_Plan_Descriptor) is
   begin
      Hexapod.Console.Put (Hexapod.Debug.Parametric_Image (Item.Ratio));
      Hexapod.Console.Put (Image (Item.LF));
      Hexapod.Console.Put (Image (Item.LM));
      Hexapod.Console.Put (Image (Item.LH));
      Hexapod.Console.Put (Image (Item.RF));
      Hexapod.Console.Put (Image (Item.RM));
      Hexapod.Console.Put (Image (Item.RH));
   end Put;

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

   -------------------------
   -- Initialize_Hardware --
   -------------------------

   procedure Initialize_Hardware is
   begin
      --  Initiazlie PCA9685 PWM controllers

      declare
         Success : Boolean := True;

      begin
         Hexapod.Hardware.Left_PWM_Controller.Initialize (Success);

         if not Success then
            Console.Put_Line
              ("FAIL: Servo Motors Controller (Left): initialization failed.");
         end if;
      end;

      declare
         Success : Boolean := True;

      begin
         Hexapod.Hardware.Right_PWM_Controller.Initialize (Success);

         if not Success then
            Console.Put_Line
              ("FAIL: Servo Motors Controller (Right): initialization failed.");
         end if;
      end;
   end Initialize_Hardware;

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
      BBF.Delays.Delay_For (A0B.Time.Milliseconds (Integer (Wait)));
      M_1.Channel.Set (0, A);
      BBF.Delays.Delay_For (A0B.Time.Milliseconds (Integer (Wait)));
      M_2.Channel.Set (0, B);
      BBF.Delays.Delay_For (A0B.Time.Milliseconds (Integer (Wait)));
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
      Trajectory.Steps.Planner.Compute_Step (0.070, 0.000, 0.030, Step_Plan);
      Trajectory.Steps.Executor.Compute_Position
        (LF_Base     => LF_Base,
         LM_Base     => LM_Base,
         LH_Base     => LH_Base,
         RF_Base     => RF_Base,
         RM_Base     => RM_Base,
         RH_Base     => RH_Base,
         Plan        => Step_Plan,
         Fase        => 1.0,
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

   -------------------
   -- Register_Task --
   -------------------

   procedure Register_Task is
   begin
      A0B.Tasking.Register_Thread (TCB, Task_Subprogram'Access, 16#400#);
   end Register_Task;

   --------------
   -- Set_Gait --
   --------------

   procedure Set_Gait (Gait : Gait_Kind) is
   begin
      case Gait is
         when Stop   =>
            Trajectory.Steps.Planner.Transition
              (Trajectory.Steps.Planner.Stop_Gait);

         when Wave   =>
            Trajectory.Steps.Planner.Transition
              (Trajectory.Steps.Planner.Wave_Gait);

         when Quadro =>
            Trajectory.Steps.Planner.Transition
              (Trajectory.Steps.Planner.Quadro_Gait);

         when Tripod =>
            Trajectory.Steps.Planner.Transition
              (Trajectory.Steps.Planner.Tripod_Gait);
      end case;
   end Set_Gait;

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
      --  Hexapod.Console.Put ("*");

      Cycle_Time := @ + Tick_Duration;

      if Cycle_Time >= Cycle then
         Cycle_Time := 0.0;
         Trajectory.Steps.Planner.Compute_Step
           (0.070, 0.000, 0.030, Step_Plan);

         Put (Step_Plan);
         Hexapod.Console.New_Line;
      end if;

      Trajectory.Steps.Executor.Compute_Position
        (LF_Base     => LF_Base,
         LM_Base     => LM_Base,
         LH_Base     => LH_Base,
         RF_Base     => RF_Base,
         RM_Base     => RM_Base,
         RH_Base     => RH_Base,
         Plan        => Step_Plan,
         Fase        => Cycle_Time / Cycle,
         LF_Position => LF_Position,
         LM_Position => LM_Position,
         LH_Position => LH_Position,
         RF_Position => RF_Position,
         RM_Position => RM_Position,
         RH_Position => RH_Position);

      Hexapod.Hardware.Left_Servo_Controller.Start_Transaction;
      Hexapod.Hardware.Right_Servo_Controller.Start_Transaction;

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

      Hexapod.Hardware.Left_Servo_Controller.Commit_Transaction;
      Hexapod.Hardware.Right_Servo_Controller.Commit_Transaction;
   end Step;

   ---------------------
   -- Task_Subprogram --
   ---------------------

   procedure Task_Subprogram is
   begin
      Initialize_Hardware;
      --  ??? It is unclear how to initialize hardware in tasking environment.
      --  Code was moved here just to avoid crash at startup and need to be
      --  reviewed.

      declare
         use type A0B.Time.Monotonic_Time;
         use type A0B.Time.Time_Span;

         Tick_Duration : constant A0B.Time.Time_Span :=
           A0B.Time.Milliseconds (1000 / Hexapod.Movement.Ticks);
         Next_Tick     : A0B.Time.Monotonic_Time := A0B.Time.Clock;

      begin
         loop
            if Next_Tick <= A0B.Time.Clock then
               if A0B.Time.Clock - Next_Tick > Tick_Duration then
                  Console.Put ("-");
               end if;

               Next_Tick := A0B.Time.Clock + Tick_Duration;

               if Movement_Enabled then
                  Step;
               end if;
            end if;
         end loop;
      end;
   end Task_Subprogram;

end Hexapod.Movement;
