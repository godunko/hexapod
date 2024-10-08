--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with A0B.PlayStation2_Controllers.Communications;
with A0B.PlayStation2_Controllers.Protocol;
with A0B.Tasking;
with A0B.Time.Clock;
with A0B.Types;

with CGK.Reals;

with BBF.Awaits;

with Hexapod.Console;
with Hexapod.Hardware;
with Hexapod.Movement;
with Hexapod.Remote_Control.Configuration;

package body Hexapod.Remote_Control is

   Polling_Rate : constant := 100;

   PS2C : A0B.PlayStation2_Controllers.Communications.Communication_Driver
            (Hexapod.Remote_Control.Configuration.SPI_Device'Access,
             Hexapod.Remote_Control.Configuration.ACK_Pin'Access);

   Transfer_Status : aliased A0B.Operation_Status;
   Transmit_Buffer :
     A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
   Receive_Buffer  :
     A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
   Configured      : Boolean := False;

   Task_Control : aliased A0B.Tasking.Task_Control_Block;

   procedure Task_Subprogram;
   --  Main subprogram of the remote controller handling task

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Hexapod.Remote_Control.Configuration.Initialize;

      PS2C.Initialize;
   end Initialize;

   ----------
   -- Poll --
   ----------

   procedure Poll is
      Await   : aliased BBF.Awaits.Await;
      Success : Boolean := True;

   begin
      A0B.PlayStation2_Controllers.Protocol.Packet_Encoder.Poll
        (Transmit_Buffer);

      PS2C.Transfer
        (Transmit_Buffer => Transmit_Buffer,
         Receive_Buffer  => Receive_Buffer,
         Status          => Transfer_Status,
         On_Completed    => BBF.Awaits.Create_Callback (Await),
         Success         => Success);

      if not Success then
         return;
      end if;

      BBF.Awaits.Suspend_Till_Callback (Await);
   end Poll;

   -------------------
   -- Register_Task --
   -------------------

   procedure Register_Task is
   begin
      A0B.Tasking.Register_Thread
        (Task_Control, Task_Subprogram'Access, 16#400#);
   end Register_Task;

   ---------------------
   -- Task_Subprogram --
   ---------------------

   procedure Task_Subprogram is
      use type A0B.Operation_Status;
      use type A0B.Time.Monotonic_Time;
      use type A0B.Types.Unsigned_8;
      use type CGK.Reals.Real;

      function Map (Value : A0B.Types.Unsigned_8) return CGK.Reals.Real;

      ---------
      -- Map --
      ---------

      function Map (Value : A0B.Types.Unsigned_8) return CGK.Reals.Real is
      begin
         if Value <= 16#80# then
            return 1.0 - (1.0 / 128.0) * CGK.Reals.Real (Value);

         else
            return -(1.0 / 127.0) * CGK.Reals.Real (Value - 16#80#);
         end if;
      end Map;

      Next             : A0B.Time.Monotonic_Time := A0B.Time.Clock;
      Polling_Interval : constant A0B.Time.Time_Span :=
        A0B.Time.Milliseconds (1_000 / Polling_Rate);
      State            : A0B.PlayStation2_Controllers.Controller_State;

      V_X              : CGK.Reals.Real;
      V_Y              : CGK.Reals.Real;
      V_W              : CGK.Reals.Real;

   begin
      loop
         --  Delay till next polling

         Next := Next + Polling_Interval;
         A0B.Tasking.Delay_Until (Next);

         if not Configured then
            declare
               Await   : aliased BBF.Awaits.Await;
               Success : Boolean := True;

            begin
               --  Turns controller into configuration mode

               A0B.PlayStation2_Controllers.Protocol.Packet_Encoder
                 .Enter_Configuration_Mode (Transmit_Buffer);

               PS2C.Transfer
                 (Transmit_Buffer => Transmit_Buffer,
                  Receive_Buffer  => Receive_Buffer,
                  Status          => Transfer_Status,
                  On_Completed    => BBF.Awaits.Create_Callback (Await),
                  Success         => Success);

               if Success then
                  BBF.Awaits.Suspend_Till_Callback (Await);

                  Success := Transfer_Status = A0B.Success;
               end if;

               --  Switch controller into analog joysticts mode and disable
               --  switch to digital mode by the user.

               A0B.PlayStation2_Controllers.Protocol.Packet_Encoder
                 .Enable_Analog_Mode (Transmit_Buffer, True);

               PS2C.Transfer
                 (Transmit_Buffer => Transmit_Buffer,
                  Receive_Buffer  => Receive_Buffer,
                  Status          => Transfer_Status,
                  On_Completed    => BBF.Awaits.Create_Callback (Await),
                  Success         => Success);

               if Success then
                  BBF.Awaits.Suspend_Till_Callback (Await);

                  Success := Transfer_Status = A0B.Success;
               end if;

               --  Turns controller back into polling mode

               A0B.PlayStation2_Controllers.Protocol.Packet_Encoder
                 .Leave_Configuration_Mode (Transmit_Buffer);

               PS2C.Transfer
                 (Transmit_Buffer => Transmit_Buffer,
                  Receive_Buffer  => Receive_Buffer,
                  Status          => Transfer_Status,
                  On_Completed    => BBF.Awaits.Create_Callback (Await),
                  Success         => Success);

               if Success then
                  BBF.Awaits.Suspend_Till_Callback (Await);

                  Success := Transfer_Status = A0B.Success;
               end if;

               --  Check whether configuration was successful.

               if Success then
                  Configured := True;
                  Hexapod.Console.Put_Line
                    ("PS2C: communication established, controller configured");
               end if;
            end;

         else
            Poll;

            if Transfer_Status /= A0B.Success then
               Configured := False;
               Hexapod.Console.Put_Line ("PS2C: communication failure");

            else
               A0B.PlayStation2_Controllers.Protocol.Packet_Decoder.Poll
                 (Receive_Buffer, State);

               if State.Triangle_Button /= 0 then
                  Hexapod.Console.Put_Line ("PS2C: activate movement");
                  Hexapod.Hardware.Enable_Motors_Power;
                  Hexapod.Movement.Configure;
               end if;

               if State.Circle_Button /= 0 then
                  Hexapod.Console.Put_Line ("PS2C: turn off motors");
                  Hexapod.Hardware.Disable_Motors_Power;
                  --  Hexapod.Movement.Movement_Enabled := False;
               end if;

               V_X := Map (State.Right_Joystick_Vertical);
               V_Y := Map (State.Right_Joystick_Horizontal);
               V_W := Map (State.Left_Joystick_Horizontal);

               Hexapod.Movement.Set_Relative_Velocity
                 (V_X => V_X, V_Y => V_Y, V_W => V_W);
            end if;
         end if;
      end loop;
   end Task_Subprogram;

end Hexapod.Remote_Control;
