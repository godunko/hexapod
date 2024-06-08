--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with System.Machine_Code;

with A0B.ATSAM3X8E.PIO.PIOA;
with A0B.ATSAM3X8E.PIO.PIOB;
with A0B.ATSAM3X8E.USART.Generic_USART1_SPI;
with A0B.Callbacks.Generic_Non_Dispatching;
with A0B.PlayStation2_Controllers.Protocol;
with A0B.Tasking;
with A0B.Time.Clock;
with A0B.Timer;
with A0B.Types.GCC_Builtins;

with BBF.Awaits;
with Hexapod.Console;
with Hexapod.Remote_Control.PS2C;

package body Hexapod.Remote_Control is

   Polling_Rate : constant := 250;

   package USART1_SPI is
     new A0B.ATSAM3X8E.USART.Generic_USART1_SPI
           (MISO =>
              A0B.ATSAM3X8E.PIO.RXD1_Line'Class
                (A0B.ATSAM3X8E.PIO.PIOA.PA12),
            MOSI =>
              A0B.ATSAM3X8E.PIO.TXD1_Line'Class
                (A0B.ATSAM3X8E.PIO.PIOA.PA13),
            SCK =>
              A0B.ATSAM3X8E.PIO.SCK1_Line'Class
                (A0B.ATSAM3X8E.PIO.PIOA.PA16),
            NSS =>
              A0B.ATSAM3X8E.PIO.RTS1_Line'Class
                (A0B.ATSAM3X8E.PIO.PIOA.PA14));

   Device     : aliased A0B.ATSAM3X8E.USART.SPI_Slave_Device
     (USART1_SPI.USART1_SPI'Access);
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

   --------------
   -- Exchange --
   --------------

   procedure Exchange is
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
   end Exchange;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      USART1_SPI.USART1_SPI.Configure;

      --  Device.Configure;

      Controller.Initialize;

      ACQ.Configure_EXTI (A0B.ATSAM3X8E.PIO.Rising_Edge, Pullup => True);
   end Initialize;

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

      Next             : A0B.Time.Monotonic_Time := A0B.Time.Clock;
      Polling_Interval : constant A0B.Time.Time_Span :=
        A0B.Time.Milliseconds (1_000 / Polling_Rate);

   begin
      loop
         Next := Next + Polling_Interval;

         A0B.Tasking.Delay_Until (Next);
         Exchange;
      end loop;
   end Task_Subprogram;

end Hexapod.Remote_Control;
