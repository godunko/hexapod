--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with A0B.ATSAM3X8E.PIO.PIOB;
with A0B.ATSAM3X8E.USART.SPI;
with A0B.PlayStation2_Controllers.Protocol;
with A0B.Tasking;
with A0B.Time.Clock;
with A0B.Types;

with CGK.Reals;

with BBF.Awaits;

with Hexapod.Console;
with Hexapod.Hardware;
with Hexapod.Movement;
with Hexapod.Remote_Control.Internals;
with Hexapod.Remote_Control.PS2C;

package body Hexapod.Remote_Control is

   Polling_Rate : constant := 100;

   Device     : aliased A0B.ATSAM3X8E.USART.SPI_Slave_Device
     (A0B.ATSAM3X8E.USART.SPI.USART1_SPI'Access);
   ACQ        : A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class
     renames A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class
       (A0B.ATSAM3X8E.PIO.PIOB.PB26);
   Controller : Hexapod.Remote_Control.PS2C.Communication_Driver
                  (Device'Access, ACQ'Access);

   Transmit_Buffer :
     A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
   Receive_Buffer  :
     A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;

   Task_Control : aliased A0B.Tasking.Task_Control_Block;

   procedure Task_Subprogram;
   --  Main subprogram of the remote controller handling task

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      A0B.ATSAM3X8E.USART.SPI.USART1_SPI.Configure;
      --  Device.Configure;

      Controller.Initialize;

      ACQ.Configure_EXTI (A0B.ATSAM3X8E.PIO.Rising_Edge, Pullup => True);
   end Initialize;

   ----------
   -- Poll --
   ----------

   procedure Poll is
      Await   : aliased BBF.Awaits.Await;
      Success : Boolean := True;

   begin
      A0B.PlayStation2_Controllers.Protocol.Packet_Builder.Poll
        (Transmit_Buffer);

      Controller.Transfer
        (Transmit_Buffer => Transmit_Buffer,
         Receive_Buffer  => Receive_Buffer,
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

   begin
      loop
         Poll;

         Internals.Get_State (Receive_Buffer, State);

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

         Hexapod.Movement.Set_Relative_Velocity (V_X, V_Y);

         --  Delay till next polling

         Next := Next + Polling_Interval;
         A0B.Tasking.Delay_Until (Next);
      end loop;
   end Task_Subprogram;

end Hexapod.Remote_Control;
