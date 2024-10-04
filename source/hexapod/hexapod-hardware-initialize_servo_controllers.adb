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
      Hexapod.Hardware.PWM1.Initialize
        (BBF.Awaits.Create_Callback (Await), Success);

      if Success then
         BBF.Awaits.Suspend_Till_Callback (Await);
         Success := Hexapod.Hardware.PWM1.State = A0B.PCA9685.Drivers.Ready;
      end if;

      if not Success then
         Console.Put_Line ("PWM1: initialization failed.");
      end if;
   end;

   declare
      Success : Boolean := True;
      Await   : aliased BBF.Awaits.Await;

   begin
      Hexapod.Hardware.PWM2.Initialize
        (BBF.Awaits.Create_Callback (Await), Success);

      if Success then
         BBF.Awaits.Suspend_Till_Callback (Await);
         Success := Hexapod.Hardware.PWM2.State = A0B.PCA9685.Drivers.Ready;
      end if;

      if not Success then
         Console.Put_Line ("PWM2: initialization failed.");
      end if;
   end;

   declare
      Success : Boolean := True;
      Await   : aliased BBF.Awaits.Await;

   begin
      Hexapod.Hardware.PWM1.Configure
        (Frequency => PWM_Frequency,
         Finished  => BBF.Awaits.Create_Callback (Await),
         Success   => Success);

      if Success then
         BBF.Awaits.Suspend_Till_Callback (Await);
         Success := Hexapod.Hardware.PWM1.State = A0B.PCA9685.Drivers.Ready;
      end if;

      if not Success then
         Console.Put_Line ("PWM1: configuration failed.");
      end if;
   end;

   declare
      Success : Boolean := True;
      Await   : aliased BBF.Awaits.Await;

   begin
      Hexapod.Hardware.PWM2.Configure
        (Frequency => PWM_Frequency,
         Finished  => BBF.Awaits.Create_Callback (Await),
         Success   => Success);

      if Success then
         BBF.Awaits.Suspend_Till_Callback (Await);
         Success := Hexapod.Hardware.PWM2.State = A0B.PCA9685.Drivers.Ready;
      end if;

      if not Success then
         Console.Put_Line ("PWM2: configuration failed.");
      end if;
   end;
end Hexapod.Hardware.Initialize_Servo_Controllers;
