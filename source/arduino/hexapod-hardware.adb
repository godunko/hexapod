--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with System.Storage_Elements;

with BBF.GPIO;

with Console;

package body Hexapod.Hardware is

   CHIP_FREQ_CPU_MAX : constant := 84_000_000;
   --  XXX Should be computed based on current settings of the chip

   PWM_Frequency : constant := 100;

   LED : not null access BBF.GPIO.Pin'Class renames BBF.Board.Pin_13_LED;

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);

   ---------------------------
   -- Configure_Controllers --
   ---------------------------

   procedure Configure_Controllers is
   begin
      declare
         Success : Boolean := True;

      begin
         Servo_Controller_Left.Configure
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
         Servo_Controller_Right.Configure
           (Frequency => PWM_Frequency,
            Success   => Success);

         if not Success then
            Console.Put_Line
              ("FAIL: Servo Motors Controller (Right): configuration failed.");
         end if;
      end;
   end Configure_Controllers;

   -------------------------
   -- Initialize_Hardware --
   -------------------------

   procedure Initialize_Hardware is
   begin
      --  First, turn off onboard LED (used by last chance handler)

      LED.Set_Direction (BBF.GPIO.Output);
      LED.Set (False);

      --  Second, initialize delay controller (used by last chance handler)

      BBF.Board.Initialize_Delay_Controller;

      --  Initialize console and output logo

      Console.Initialize (CHIP_FREQ_CPU_MAX);

      --  Initialize I2C master controllers

      BBF.Board.I2C.Initialize_I2C_0;
      BBF.Board.I2C.Initialize_I2C_1;

      --  Initiazlie PCA9685 PWM controllers

      declare
         Success : Boolean := True;

      begin
         Servo_Controller_Left.Initialize (Success);

         if not Success then
            Console.Put_Line
              ("FAIL: Left Servo Motors Controller initialization failed.");
         end if;
      end;

      declare
         Success : Boolean := True;

      begin
         Servo_Controller_Right.Initialize (Success);

         if not Success then
            Console.Put_Line
              ("FAIL: Right Servo Motors Controller initialization failure.");
         end if;
      end;
   end Initialize_Hardware;

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      J : System.Storage_Elements.Integer_Address :=
        System.Storage_Elements.To_Integer (Msg);
      L : constant String := Integer'Image (Line);

   begin
      --  XXX Last chance handler can be called from the interrupt handler
      --  when CPU interrupts are disabled. In such case asynchronous write
      --  operation to console can't be used, and code should cancel UART IO
      --  and use synchronous API. Thus, looks reasonable to move last chance
      --  handler to Console package.

      Console.Put ("ADA EXCEPTION at ");

      loop
         declare
            use type System.Storage_Elements.Integer_Address;

            C : constant Character
              with Import,
                   Convention => Ada,
                   Address    => System.Storage_Elements.To_Address (J);
            S : constant String (1 .. 1) := (1 => C);

         begin
            exit when C = ASCII.NUL;

            Console.Put (S);

            J := @ + 1;
         end;
      end loop;

      Console.Put_Line (":" & L (L'First + 1 .. L'Last));

      loop
         LED.Set (False);
         BBF.Board.Delay_Controller.Delay_Milliseconds (500);

         LED.Set (True);
         BBF.Board.Delay_Controller.Delay_Milliseconds (500);
      end loop;
   end Last_Chance_Handler;

end Hexapod.Hardware;
