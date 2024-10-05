--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  pragma Restrictions (No_Elaboration_Code);

with Ada.Numerics;
with Interfaces;

with A0B.ARMv7M.DWT;
with A0B.Callbacks.Generic_Parameterless;
with A0B.Tasking;
with A0B.Time.Clock;
with A0B.Types;

with BBF.Delays;
with A0B.PCA9685;

with Kinematics;
with Legs.State;
with Legs.Gait_Generator;
with Legs.Trajectory;
with Legs.Trajectory_Generator;
with Legs.Workspace;

with Hexapod.Console;
--  with Hexapod.Debug;
with Hexapod.Hardware.Initialize_Servo_Controllers;
with Hexapod.Parameters.Control_Cycle;

package body Hexapod.Movement is

   use type CGK.Reals.Real;

   LF_Posture  : Kinematics.Posture
     renames Legs.State.Posture (Legs.Left_Front);
   LM_Posture  : Kinematics.Posture
     renames Legs.State.Posture (Legs.Left_Middle);
   LH_Posture  : Kinematics.Posture
     renames Legs.State.Posture (Legs.Left_Hind);
   RF_Posture  : Kinematics.Posture
     renames Legs.State.Posture (Legs.Right_Front);
   RM_Posture  : Kinematics.Posture
     renames Legs.State.Posture (Legs.Right_Middle);
   RH_Posture  : Kinematics.Posture
     renames Legs.State.Posture (Legs.Right_Hind);

   TCB : aliased A0B.Tasking.Task_Control_Block;

   procedure Task_Subprogram;
   --  Task thread subprogram.

   type Motor_Descriptor is record
      Channel   : not null access A0B.PCA9685.PCA9685_Channel'Class;
      Min_PWM   : A0B.PCA9685.Value_Type;
      Max_PWM   : A0B.PCA9685.Value_Type;
      Min_Angle : CGK.Reals.Real;
      Max_Angle : CGK.Reals.Real;
   end record;

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

   Body_Height : constant := 0.070;

   type States is (Initial, Configuration, Active);

   State : States := Initial;

   procedure Prepare;

   procedure On_PWM1_Completed;

   procedure On_PWM2_Completed;

   package On_PWM1_Completed_Callbacks is
     new A0B.Callbacks.Generic_Parameterless (On_PWM1_Completed);

   package On_PWM2_Completed_Callbacks is
     new A0B.Callbacks.Generic_Parameterless (On_PWM2_Completed);

   ---------------
   -- Configure --
   ---------------

   procedure Configure is
   begin
      if State = Initial then
         State := Configuration;
      end if;
   end Configure;

   -----------
   -- Image --
   -----------

   --  function Image (Item : Trajectory.Steps.Leg_Step_Plan_Descriptor) return String is
   --     use type Trajectory.Steps.Stage_Kind;
   --
   --  begin
   --     return
   --       (if Item.Stage = Trajectory.Steps.Strait then "  _ " else "  ^ ")
   --       & (if Item.Stage = Trajectory.Steps.Swing then
   --             Hexapod.Debug.Coordinate_Image (Item.AEP_X)
   --             & Hexapod.Debug.Coordinate_Image (Item.AEP_Y)
   --             & Hexapod.Debug.Coordinate_Image (Item.PEP_X)
   --             & Hexapod.Debug.Coordinate_Image (Item.PEP_Y)
   --          else "");
   --  end Image;

   --  function Image (Item : Trajectory.Steps.Step_Plan_Descriptor) return String is
   --  procedure Put (Item : Trajectory.Steps.Step_Plan_Descriptor) is
   --  begin
   --     Hexapod.Console.Put_Line
   --       (Hexapod.Debug.Coordinate_Image (Body_Velocity_X)
   --        & Hexapod.Debug.Coordinate_Image (Body_Velocity_Y));
   --     Hexapod.Console.Put (Hexapod.Debug.Parametric_Image (Item.Ratio));
   --     Hexapod.Console.Put (Image (Item.LF));
   --     Hexapod.Console.Put (Image (Item.LM));
   --     Hexapod.Console.Put (Image (Item.LH));
   --     Hexapod.Console.Put (Image (Item.RF));
   --     Hexapod.Console.Put (Image (Item.RM));
   --     Hexapod.Console.Put (Image (Item.RH));
   --  end Put;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Legs.Initialize;
      Legs.Workspace.Compute (Body_Height);
      Legs.Trajectory.Initialize;
      Legs.Trajectory_Generator.Initialize;
      Legs.Gait_Generator.Initialize;
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
         Angle      : CGK.Reals.Real) return A0B.PCA9685.Value_Type;

      ---------
      -- Map --
      ---------

      function Map
        (Descriptor : Motor_Descriptor;
         Angle      : CGK.Reals.Real) return A0B.PCA9685.Value_Type
      is
         use type A0B.PCA9685.Value_Type;

         PWM_Range : constant CGK.Reals.Real :=
           CGK.Reals.Real (Descriptor.Max_PWM - Descriptor.Min_PWM);

      begin
         return
           A0B.PCA9685.Value_Type
             ((Angle - Descriptor.Min_Angle)
              / ( Descriptor.Max_Angle - Descriptor.Min_Angle) * PWM_Range
                 + CGK.Reals.Real (Descriptor.Min_PWM));
      end Map;

      A : A0B.PCA9685.Value_Type;
      B : A0B.PCA9685.Value_Type;
      C : A0B.PCA9685.Value_Type;

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

   -----------------------
   -- On_PWM1_Completed --
   -----------------------

   procedure On_PWM1_Completed is
      Success : Boolean := True;

   begin
      --  if Transaction_Status /= A0B.Success then
      --     raise Program_Error;
      --  end if;

      Hexapod.Hardware.PWM2_Controller.Commit_Transaction
        (Finished => On_PWM2_Completed_Callbacks.Create_Callback,
         Success  => Success);

      if not Success then
         raise Program_Error;
      end if;
   end On_PWM1_Completed;

   -----------------------
   -- On_PWM2_Completed --
   -----------------------

   procedure On_PWM2_Completed is
   begin
      null;
      --  if Transaction_Status /= A0B.Success then
      --     raise Program_Error;
      --  end if;
   end On_PWM2_Completed;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      --  LF & RH

      Move (LF_Posture, LF_M_1, LF_M_2, LF_M_3, 50);
      Move (RH_Posture, RH_M_1, RH_M_2, RH_M_3, 50);

      --  LM & RM

      Move (LM_Posture, LM_M_1, LM_M_2, LM_M_3, 50);
      Move (RM_Posture, RM_M_1, RM_M_2, RM_M_3, 50);

      --  LH & RF

      Move (LH_Posture, LH_M_1, LH_M_2, LH_M_3, 50);
      Move (RF_Posture, RF_M_1, RF_M_2, RF_M_3, 50);
   end Prepare;

   -------------------
   -- Register_Task --
   -------------------

   procedure Register_Task is
   begin
      A0B.Tasking.Register_Thread (TCB, Task_Subprogram'Access, 16#400#);
   end Register_Task;

   ---------------------------
   -- Set_Relative_Velocity --
   ---------------------------

   procedure Set_Relative_Velocity
     (V_X : CGK.Reals.Real;
      V_Y : CGK.Reals.Real;
      V_W : CGK.Reals.Real) is
   begin
      Legs.Gait_Generator.Set_Velocity (RVX => V_X, RVY => V_Y, RVW => V_W);
   end Set_Relative_Velocity;

   ----------
   -- Step --
   ----------

   procedure Step is
      Success : Boolean := True;

   begin
      --  Update desired legs posture.

      Legs.Trajectory_Generator.Tick;

      Hexapod.Hardware.PWM1_Controller.Start_Transaction;
      Hexapod.Hardware.PWM2_Controller.Start_Transaction;

      Move (LF_Posture, LF_M_1, LF_M_2, LF_M_3, 0);
      Move (LM_Posture, LM_M_1, LM_M_2, LM_M_3, 0);
      Move (LH_Posture, LH_M_1, LH_M_2, LH_M_3, 0);
      Move (RF_Posture, RF_M_1, RF_M_2, RF_M_3, 0);
      Move (RM_Posture, RM_M_1, RM_M_2, RM_M_3, 0);
      Move (RH_Posture, RH_M_1, RH_M_2, RH_M_3, 0);

      Hexapod.Hardware.PWM1_Controller.Commit_Transaction
        (Finished => On_PWM1_Completed_Callbacks.Create_Callback,
         Success  => Success);

      if not Success then
         raise Program_Error;
      end if;

      --  Compute gait.

      Legs.Gait_Generator.Tick;
   end Step;

   ---------------------
   -- Task_Subprogram --
   ---------------------

   Cur_Cycles : A0B.Types.Unsigned_32 := 0 with Volatile;
   Max_Cycles : A0B.Types.Unsigned_32 := 0 with Volatile;
   Min_Cycles : A0B.Types.Unsigned_32 := A0B.Types.Unsigned_32'Last with Volatile;

   procedure Task_Subprogram is
   begin
      Hexapod.Hardware.Initialize_Servo_Controllers;
      --  ??? It is unclear how to initialize hardware in tasking environment.
      --  Code was moved here just to avoid crash at startup and need to be
      --  reviewed.

      declare
         use type A0B.Time.Monotonic_Time;
         use type A0B.Time.Time_Span;

         Next_Tick : A0B.Time.Monotonic_Time := A0B.Time.Clock;

      begin
         loop
            case State is
               when Initial =>
                  null;

               when Configuration =>
                  Prepare;

                  State     := Active;
                  Next_Tick := A0B.Time.Clock;
                  --  Restart tick start time, prepare requires move time than
                  --  single tick.

               when Active =>
                  if A0B.Time.Clock - Next_Tick
                       > A0B.Time.To_Time_Span
                           (Hexapod.Parameters.Control_Cycle.Tick_Duration)
                  then
                     Console.Put ("-");
                  end if;

                  A0B.ARMv7M.DWT.DWT_CTRL.CYCCNTENA := True;
                  A0B.ARMv7M.DWT.DWT_CYCCNT.CYCCNT  := 0;

                  Step;

                  Cur_Cycles := A0B.ARMv7M.DWT.DWT_CYCCNT.CYCCNT;
                  Max_Cycles := A0B.Types.Unsigned_32'Max (@, Cur_Cycles);
                  Min_Cycles := A0B.Types.Unsigned_32'Min (@, Cur_Cycles);
            end case;

            Next_Tick :=
              @ + A0B.Time.To_Time_Span
                    (Hexapod.Parameters.Control_Cycle.Tick_Duration);
            A0B.Tasking.Delay_Until (Next_Tick);
         end loop;
      end;
   end Task_Subprogram;

end Hexapod.Movement;
