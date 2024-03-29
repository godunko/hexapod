--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with System.Storage_Elements;
pragma Warnings (Off, """System.Semihosting"" is an internal GNAT unit");
with System.Semihosting;

with BBF.GPIO;
with BBF.HPL.PMC;
with BBF.Drivers.MPU;

with Hexapod.Console;

package body Hexapod.Hardware is

   PWM_Frequency : constant := 321;
   --  Closest value for supported by PCA 9685 controller.

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

      declare
         Success : Boolean := True;

      begin
         --  Body_Position_Sensor.Configure
         --    (BBF.Board.Delay_Controller,
         --     BBF.Drivers.MPU.FSR_2G,
         --     BBF.Drivers.MPU.FSR_2000DPS,
         --     True,
         --     True,
         --     198,
         --     Success);
         Body_Position_Sensor.Configure
           (BBF.Board.Delay_Controller,
            200,
            Success);

         if not Success then
            Console.Put_Line
              ("FAIL: Body Position Sensor: configuration failed.");
         end if;

         Body_Position_Sensor.Enable (BBF.Board.Delay_Controller);
      end;
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

      Console.Initialize;

      --  Configure motor power relay control pin.

      Left_Motor_Power_Relay.Set (True);
      Right_Motor_Power_Relay.Set (True);
      Left_Motor_Power_Relay.Set_Direction (BBF.GPIO.Output);
      Right_Motor_Power_Relay.Set_Direction (BBF.GPIO.Output);

      --  Initialize real time clock

      BBF.Board.Initialize_Real_Time_Clock_Controller;

      --  Initialize I2C master controllers

      BBF.Board.I2C.Initialize_I2C_0;
      BBF.Board.I2C.Initialize_I2C_1;

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

      declare
         Success : Boolean := True;

      begin
         BBF.HPL.PMC.Enable_Peripheral_Clock
           (BBF.HPL.Parallel_IO_Controller_A);
         BBF.HPL.PMC.Enable_Peripheral_Clock
           (BBF.HPL.Parallel_IO_Controller_C);
         --  XXX Must be moved out! Clock should be enabled when interrupt
         --  handling on the pin is enabled.

         Body_Position_Sensor.Initialize (BBF.Board.Delay_Controller, Success);

         if not Success then
            Console.Put_Line
              ("FAIL: Body Position Sensor initialization failure.");
         end if;
      end;
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
         BBF.Board.Delay_Controller.Delay_Milliseconds (500);

         LED.Set (True);
         BBF.Board.Delay_Controller.Delay_Milliseconds (500);
      end loop;
   end Last_Chance_Handler;

end Hexapod.Hardware;
