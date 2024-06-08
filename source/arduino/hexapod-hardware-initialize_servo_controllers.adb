--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Initialization and configuration of the servo motors' controllers.

with Hexapod.Console;

procedure Hexapod.Hardware.Initialize_Servo_Controllers is
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

   declare
      Success : Boolean := True;

   begin
      Hexapod.Hardware.Left_PWM_Controller.Configure
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
      Hexapod.Hardware.Right_PWM_Controller.Configure
        (Frequency => PWM_Frequency,
         Success   => Success);

      if not Success then
         Console.Put_Line
           ("FAIL: Servo Motors Controller (Right): configuration failed.");
      end if;
   end;
end Hexapod.Hardware.Initialize_Servo_Controllers;
