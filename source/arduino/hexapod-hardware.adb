--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Conversion;
with System.Storage_Elements;
pragma Warnings (Off, """System.Semihosting"" is an internal GNAT unit");
with System.Semihosting;

with A0B.ARMv7M.System_Control_Block;
with A0B.ARMv7M.CMSIS;
with A0B.Types;

with BBF.HPL;
--  with BBF.HPL.PMC;
--  with BBF.Drivers.MPU;
with A0B.ATSAM3X8E.SVD.SYSC;

with Hexapod.Console;

package body Hexapod.Hardware is

   PWM_Frequency : constant := 321;
   --  Closest value for supported by PCA 9685 controller.

   LED : A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class renames BBF.Board.Pin_13_LED;

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);

   procedure HardFault_Handler
     with Export, Convention => C, External_Name => "HardFault_Handler";

   Fault_Message  : String :=
     "ICSR  => XXXX_XXXX" & ASCII.LF
     & "CFSR  => XXXX_XXXX" & ASCII.LF
     & "HFSR  => XXXX_XXXX" & ASCII.LF
     & "MMFAR => XXXX_XXXX" & ASCII.LF
     & "BFAR  => XXXX_XXXX" & ASCII.LF
     with Volatile;
   ICSR_Position  : constant := 10;
   CFSR_Position  : constant := ICSR_Position + 19;
   HFSR_Position  : constant := CFSR_Position + 19;
   MMFAR_Position : constant := HFSR_Position + 19;
   BFAR_Position  : constant := MMFAR_Position + 19;

   ---------------------------
   -- Configure_Controllers --
   ---------------------------

   procedure Configure_Controllers is
   begin
      declare
         Success : Boolean := True;

      begin
         Left_PWM_Controller.Configure
           (Frequency => PWM_Frequency,
            Success   => Success);

         if not Success then
            Console.Put_Line
              ("FAIL: Servo Motors Controller (Left): configuration failed.");
         end if;
      end;

      declare
         Success : Boolean := True;

      begin
         Right_PWM_Controller.Configure
           (Frequency => PWM_Frequency,
            Success   => Success);

         if not Success then
            Console.Put_Line
              ("FAIL: Servo Motors Controller (Right): configuration failed.");
         end if;
      end;

      --  declare
      --     Success : Boolean := True;

      --  begin
      --     --  Body_Position_Sensor.Configure
      --     --    (BBF.Board.Delay_Controller,
      --     --     BBF.Drivers.MPU.FSR_2G,
      --     --     BBF.Drivers.MPU.FSR_2000DPS,
      --     --     True,
      --     --     True,
      --     --     198,
      --     --     Success);
      --     Body_Position_Sensor.Configure
      --       (BBF.Board.Delay_Controller,
      --        200,
      --        Success);

      --     if not Success then
      --        Console.Put_Line
      --          ("FAIL: Body Position Sensor: configuration failed.");
      --     end if;

      --     Body_Position_Sensor.Enable (BBF.Board.Delay_Controller);
      --  end;
   end Configure_Controllers;

   --------------------------
   -- Disable_Motors_Power --
   --------------------------

   procedure Disable_Motors_Power is
   begin
      Left_Motor_Power_Relay.Set (True);
      Right_Motor_Power_Relay.Set (True);
   end Disable_Motors_Power;

   -------------------------
   -- Enable_Motors_Power --
   -------------------------

   procedure Enable_Motors_Power is
   begin
      Left_Motor_Power_Relay.Set (False);
      Right_Motor_Power_Relay.Set (False);
   end Enable_Motors_Power;

   -----------------------
   -- HardFault_Handler --
   -----------------------

   procedure HardFault_Handler is

      function To_Unsigned_32 is
        new Ada.Unchecked_Conversion
             (A0B.ARMv7M.System_Control_Block.SCB_CFSR_Register,
              A0B.Types.Unsigned_32);

      function To_Unsigned_32 is
        new Ada.Unchecked_Conversion
             (A0B.ARMv7M.System_Control_Block.SCB_HFSR_Register,
              A0B.Types.Unsigned_32);

      function To_Unsigned_32 is
        new Ada.Unchecked_Conversion
             (A0B.ARMv7M.System_Control_Block.SCB_ICSR_Register,
              A0B.Types.Unsigned_32);

      function To_Unsigned_32 is
        new Ada.Unchecked_Conversion (System.Address, A0B.Types.Unsigned_32);

      --------------
      -- Fill_Hex --
      --------------

      procedure Fill_Hex
        (Value    : A0B.Types.Unsigned_32;
         Position : Positive)
      is
         use type A0B.Types.Unsigned_32;

         To_Hex : constant
           array (A0B.Types.Unsigned_32 range 0 .. 15) of Character :=
             "0123456789ABCDEF";
         Aux    : A0B.Types.Unsigned_32 := Value;
         --  Offset : Natural := 8;

      begin
         Fault_Message (Position + 8) := To_Hex (Aux and 2#1111#);
         Aux := @ / 16;
         Fault_Message (Position + 7) := To_Hex (Aux and 2#1111#);
         Aux := @ / 16;
         Fault_Message (Position + 6) := To_Hex (Aux and 2#1111#);
         Aux := @ / 16;
         Fault_Message (Position + 5) := To_Hex (Aux and 2#1111#);
         Aux := @ / 16;
         Fault_Message (Position + 3) := To_Hex (Aux and 2#1111#);
         Aux := @ / 16;
         Fault_Message (Position + 2) := To_Hex (Aux and 2#1111#);
         Aux := @ / 16;
         Fault_Message (Position + 1) := To_Hex (Aux and 2#1111#);
         Aux := @ / 16;
         Fault_Message (Position + 0) := To_Hex (Aux and 2#1111#);
      end Fill_Hex;

   begin
      Fill_Hex
        (To_Unsigned_32 (A0B.ARMv7M.System_Control_Block.SCB.ICSR),
         ICSR_Position);
      Fill_Hex
        (To_Unsigned_32 (A0B.ARMv7M.System_Control_Block.SCB.CFSR),
         CFSR_Position);
      Fill_Hex
        (To_Unsigned_32 (A0B.ARMv7M.System_Control_Block.SCB.HFSR),
         HFSR_Position);

      if A0B.ARMv7M.System_Control_Block.SCB.CFSR.MemManage.MMARVALID then
         Fill_Hex
           (To_Unsigned_32 (A0B.ARMv7M.System_Control_Block.SCB.MMFAR),
            MMFAR_Position);
      end if;

      if A0B.ARMv7M.System_Control_Block.SCB.CFSR.BusFault.BFARVALID then
         Fill_Hex
           (To_Unsigned_32 (A0B.ARMv7M.System_Control_Block.SCB.BFAR),
            BFAR_Position);
      end if;

      loop
         null;
      end loop;
   end HardFault_Handler;

   -------------------------
   -- Initialize_Hardware --
   -------------------------

   procedure Initialize_Hardware is
   begin
      --  Disable watchdog timer.

      A0B.ATSAM3X8E.SVD.SYSC.WDT_Periph.MR :=
        (WDFIEN    => False,
         -- A Watchdog fault (underflow or error) has no effect on interrupt.
         WDRSTEN   => False,
         --  A Watchdog fault (underflow or error) has no effect on the
         --  resets.
         WDRPROC   => <>,
         WDDIS     => True,  --  Disables the Watchdog Timer.
         WDD       => <>,
         WDDBGHLT  => True,
         --  The Watchdog stops when the processor is in debug state.
         WDIDLEHLT => False,
         --  The Watchdog runs when the processor is in debug state.
         others    => <>);

      --  Turn off onboard LED (used by last chance handler)

      LED.Configure_Output;
      LED.Set (False);

      --  Initialize console and output logo

      Console.Initialize;

      --  Configure motor power relay control pin.

      Left_Motor_Power_Relay.Configure_Output;
      Left_Motor_Power_Relay.Set (True);
      Right_Motor_Power_Relay.Configure_Output;
      Right_Motor_Power_Relay.Set (True);

      --  Initialize I2C master controllers

      A0B.I2C.ATSAM3X8E_TWI.TWI0.TWI0.Configure;
      A0B.I2C.ATSAM3X8E_TWI.TWI1.TWI1.Configure;

      --  Initiazlie PCA9685 PWM controllers

      declare
         Success : Boolean := True;

      begin
         Left_PWM_Controller.Initialize (Success);

         if not Success then
            Console.Put_Line
              ("FAIL: Servo Motors Controller (Left): initialization failed.");
         end if;
      end;

      declare
         Success : Boolean := True;

      begin
         Right_PWM_Controller.Initialize (Success);

         if not Success then
            Console.Put_Line
              ("FAIL: Servo Motors Controller (Right): initialization failed.");
         end if;
      end;

      --  Initialize body position sensor

      --  declare
      --     Success : Boolean := True;

      --  begin
      --     BBF.HPL.PMC.Enable_Peripheral_Clock
      --       (BBF.HPL.Parallel_IO_Controller_A);
      --     BBF.HPL.PMC.Enable_Peripheral_Clock
      --       (BBF.HPL.Parallel_IO_Controller_C);
      --     --  XXX Must be moved out! Clock should be enabled when interrupt
      --     --  handling on the pin is enabled.

      --     Body_Position_Sensor.Initialize (BBF.Board.Delay_Controller, Success);

      --     if not Success then
      --        Console.Put_Line
      --          ("FAIL: Body Position Sensor initialization failure.");
      --     end if;
      --  end;
   end Initialize_Hardware;

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is

      function Message return String;
      --  Returns message. It might be file:line information or text message.

      --------------
      -- Location --
      --------------

      function Message return String is

         use type System.Storage_Elements.Storage_Offset;

         Line_Image : constant String := Integer'Image (Line);
         Length     : System.Storage_Elements.Storage_Offset := 0;

      begin
         --  Compute length of the text, but limit it by 78 characters.

         loop
            declare
               use type System.Storage_Elements.Integer_Address;

               C : constant Character
                 with Import,
                      Convention => Ada,
                      Address    => Msg + Length;

            begin
               exit when C = ASCII.NUL;
               exit when Length = 78;

               Length := Length + 1;
            end;
         end loop;

         --  Construct String and return result

         declare
            Result : constant String (1 .. Natural (Length))
              with Import, Address => Msg;

         begin
            if Line = 0 then
               --  Text message

               return Result;

            else
               --  file:line

               return Result & ":" & Line_Image (2 ..Line_Image'Last);
            end if;
         end;
      end Message;

   begin
      --  First, put message to hardware debug probe.

      System.Semihosting.Put
        (ASCII.CR & ASCII.LF & "ADA: " & Message & ASCII.CR & ASCII.LF);

      --  XXX Last chance handler can be called from the interrupt handler
      --  when CPU interrupts are disabled. In such case asynchronous write
      --  operation to console can't be used, and code should cancel UART IO
      --  and use synchronous API. Thus, looks reasonable to move last chance
      --  handler to Console package.

      if BBF.HPL.Is_Interrupts_Enabled then
         --  Pass all characters of the text by single call to Put. It give
         --  better chance that text will be transmitted to console.

         Console.Put
           (ASCII.CR & ASCII.LF & "ADA: " & Message & ASCII.CR & ASCII.LF);
      end if;

      loop
         LED.Set (False);
         --  BBF.Board.Delay_Controller.Delay_Milliseconds (500);

         LED.Set (True);
         --  BBF.Board.Delay_Controller.Delay_Milliseconds (500);
      end loop;
   end Last_Chance_Handler;

end Hexapod.Hardware;
