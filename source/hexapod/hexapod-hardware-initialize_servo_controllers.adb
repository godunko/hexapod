--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Initialization and configuration of the servo motors' controllers.

with BBF.Awaits;

with Hexapod.Console;

procedure Hexapod.Hardware.Initialize_Servo_Controllers is

   use type A0B.PCA9685.Drivers.State_Kind;

begin
   --  Initiazlie PCA9685 PWM controllers

   declare
      Success : Boolean := True;
      Await   : aliased BBF.Awaits.Await;

   begin
      Hexapod.Hardware.Left_PWM_Controller.Initialize
        (BBF.Awaits.Create_Callback (Await), Success);

      if Success then
         BBF.Awaits.Suspend_Till_Callback (Await);
         Success :=
           Hexapod.Hardware.Left_PWM_Controller.State
             = A0B.PCA9685.Drivers.Ready;
      end if;

      if not Success then
         Console.Put_Line
           ("FAIL: Servo Motors Controller (Left): initialization failed.");
      end if;
   end;

   declare
      Success : Boolean := True;
      Await   : aliased BBF.Awaits.Await;

   begin
      Hexapod.Hardware.Right_PWM_Controller.Initialize
        (BBF.Awaits.Create_Callback (Await), Success);

      if Success then
         BBF.Awaits.Suspend_Till_Callback (Await);
         Success :=
           Hexapod.Hardware.Right_PWM_Controller.State
             = A0B.PCA9685.Drivers.Ready;
      end if;

      if not Success then
         Console.Put_Line
           ("FAIL: Servo Motors Controller (Right): initialization failed.");
      end if;
   end;

   declare
      Success : Boolean := True;
      Await   : aliased BBF.Awaits.Await;

   begin
      Hexapod.Hardware.Left_PWM_Controller.Configure
        (Frequency => PWM_Frequency,
         Finished  => BBF.Awaits.Create_Callback (Await),
         Success   => Success);

      if Success then
         BBF.Awaits.Suspend_Till_Callback (Await);
         Success :=
           Hexapod.Hardware.Left_PWM_Controller.State
             = A0B.PCA9685.Drivers.Ready;
      end if;

      if not Success then
         Console.Put_Line
           ("FAIL: Servo Motors Controller (Left): configuration failed.");
      end if;
   end;

   declare
      Success : Boolean := True;
      Await   : aliased BBF.Awaits.Await;

   begin
      Hexapod.Hardware.Right_PWM_Controller.Configure
        (Frequency => PWM_Frequency,
         Finished  => BBF.Awaits.Create_Callback (Await),
         Success   => Success);

      if Success then
         BBF.Awaits.Suspend_Till_Callback (Await);
         Success :=
           Hexapod.Hardware.Right_PWM_Controller.State
             = A0B.PCA9685.Drivers.Ready;
      end if;

      if not Success then
         Console.Put_Line
           ("FAIL: Servo Motors Controller (Right): configuration failed.");
      end if;
   end;
end Hexapod.Hardware.Initialize_Servo_Controllers;
