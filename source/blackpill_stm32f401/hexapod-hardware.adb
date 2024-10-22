--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Conversion;
with System.Storage_Elements;
pragma Warnings (Off, """System.Semihosting"" is an internal GNAT unit");
with System.Semihosting;

with A0B.ARMv7M.SCS.SCB;
with A0B.STM32F401.GPIO.PIOC;
with A0B.STM32F401.SVD.RCC;
with A0B.Types;

with Hexapod.Console;
with Hexapod.Remote_Control;

package body Hexapod.Hardware is

   LED : A0B.STM32F401.GPIO.GPIO_Line renames A0B.STM32F401.GPIO.PIOC.PC13;

   procedure Report_Failure_State (Msg : System.Address; Line : Integer)
     with Export, Link_Name => "__hexapod_report_failure_state";
   --  Export failure state report to attach debugger to it.

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
             (A0B.ARMv7M.SCS.SCB.CFSR_Register,
              A0B.Types.Unsigned_32);

      function To_Unsigned_32 is
        new Ada.Unchecked_Conversion
             (A0B.ARMv7M.SCS.SCB.HFSR_Register,
              A0B.Types.Unsigned_32);

      function To_Unsigned_32 is
        new Ada.Unchecked_Conversion
             (A0B.ARMv7M.SCS.SCB.ICSR_Register,
              A0B.Types.Unsigned_32);

      function To_Unsigned_32 is
        new Ada.Unchecked_Conversion
              (System.Storage_Elements.Integer_Address, A0B.Types.Unsigned_32);

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
        (To_Unsigned_32 (A0B.ARMv7M.SCS.SCB.ICSR),
         ICSR_Position);
      Fill_Hex
        (To_Unsigned_32 (A0B.ARMv7M.SCS.SCB.CFSR),
         CFSR_Position);
      Fill_Hex
        (To_Unsigned_32 (A0B.ARMv7M.SCS.SCB.HFSR),
         HFSR_Position);

      if A0B.ARMv7M.SCS.SCB.CFSR.MemManage.MMARVALID then
         Fill_Hex
           (To_Unsigned_32 (A0B.ARMv7M.SCS.SCB.MMFAR),
            MMFAR_Position);
      end if;

      if A0B.ARMv7M.SCS.SCB.CFSR.BusFault.BFARVALID then
         Fill_Hex
           (To_Unsigned_32 (A0B.ARMv7M.SCS.SCB.BFAR),
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
      A0B.STM32F401.SVD.RCC.RCC_Periph.AHB1ENR.GPIOAEN  := True;
      A0B.STM32F401.SVD.RCC.RCC_Periph.AHB1ENR.GPIOBEN  := True;
      A0B.STM32F401.SVD.RCC.RCC_Periph.AHB1ENR.GPIOCEN  := True;
      A0B.STM32F401.SVD.RCC.RCC_Periph.APB2ENR.SYSCFGEN := True;

      --  Turn off onboard LED (used by last chance handler)

      LED.Configure_Output;
      LED.Set (False);

      --  Initialize console and output logo

      Console.Initialize;

      --  Initialize remote control

      Remote_Control.Initialize;

      --  Configure motor power relay control pin.

      Left_Motor_Power_Relay.Configure_Output;
      Left_Motor_Power_Relay.Set (True);
      Right_Motor_Power_Relay.Configure_Output;
      Right_Motor_Power_Relay.Set (True);

      --  Initialize I2C master controllers

      I2C1.I2C1.Configure;
      I2C2.I2C2.Configure;

      --  Initialize INT external interrupt line for body position sensor

      IMU_INT_Pin.Configure_EXTI
        (Mode => A0B.STM32F401.GPIO.Rising_Edge,
         Pull => A0B.STM32F401.GPIO.No);
   end Initialize_Hardware;

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
   begin
      --  First, turn off motors for safety.

      Disable_Motors_Power;

      --  And report failure state.

      Report_Failure_State (Msg, Line);

      --  Blink LED

      loop
         LED.Set (False);
         --  BBF.Board.Delay_Controller.Delay_Milliseconds (500);

         LED.Set (True);
         --  BBF.Board.Delay_Controller.Delay_Milliseconds (500);
      end loop;
   end Last_Chance_Handler;

   --------------------------
   -- Report_Failure_State --
   --------------------------

   procedure Report_Failure_State (Msg : System.Address; Line : Integer) is

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

      --  if BBF.HPL.Is_Interrupts_Enabled then
         --  Pass all characters of the text by single call to Put. It give
         --  better chance that text will be transmitted to console.

         Console.Put
           (ASCII.CR & ASCII.LF & "ADA: " & Message & ASCII.CR & ASCII.LF);
      --  end if;
   end Report_Failure_State;

end Hexapod.Hardware;
