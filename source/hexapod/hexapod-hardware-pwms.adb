--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Initialization and configuration of the servo motors' controllers.

with BBF.Awaits;

with Hexapod.Console;

package body Hexapod.Hardware.PWMs is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      use type A0B.PCA9685.Drivers.State_Kind;

      PWM1_Success : Boolean := True;
      PWM2_Success : Boolean := True;

   begin
      --  Initiazlie PCA9685 PWM controllers

      declare
         Await : aliased BBF.Awaits.Await;

      begin
         PWM1.Initialize
           (BBF.Awaits.Create_Callback (Await), PWM1_Success);

         if PWM1_Success then
            BBF.Awaits.Suspend_Till_Callback (Await);
            PWM1_Success := PWM1.State = A0B.PCA9685.Drivers.Ready;
         end if;

         if not PWM1_Success then
            Console.Put_Line ("PWM1: initialization failed.");
         end if;
      end;

      declare
         Await : aliased BBF.Awaits.Await;

      begin
         PWM2.Initialize
           (BBF.Awaits.Create_Callback (Await), PWM2_Success);

         if PWM2_Success then
            BBF.Awaits.Suspend_Till_Callback (Await);
            PWM2_Success := PWM2.State = A0B.PCA9685.Drivers.Ready;
         end if;

         if not PWM2_Success then
            Console.Put_Line ("PWM2: initialization failed.");
         end if;
      end;

      if PWM1_Success then
         declare
            Await : aliased BBF.Awaits.Await;

         begin
            PWM1.Configure
              (Frequency => PWM_Frequency,
               Finished  => BBF.Awaits.Create_Callback (Await),
               Success   => PWM1_Success);

            if PWM1_Success then
               BBF.Awaits.Suspend_Till_Callback (Await);
               PWM1_Success := PWM1.State = A0B.PCA9685.Drivers.Ready;
            end if;

            if not PWM1_Success then
               Console.Put_Line ("PWM1: configuration failed.");

            else
               Console.Put_Line ("PWM1: initialized and configured");
            end if;
         end;
      end if;

      if PWM2_Success then
         declare
            Await : aliased BBF.Awaits.Await;

         begin
            PWM2.Configure
              (Frequency => PWM_Frequency,
               Finished  => BBF.Awaits.Create_Callback (Await),
               Success   => PWM2_Success);

            if PWM2_Success then
               BBF.Awaits.Suspend_Till_Callback (Await);
               PWM2_Success := PWM2.State = A0B.PCA9685.Drivers.Ready;
            end if;

            if not PWM2_Success then
               Console.Put_Line ("PWM2: configuration failed.");

            else
               Console.Put_Line ("PWM2: initialized and configured");
            end if;
         end;
      end if;
   end Initialize;

end Hexapod.Hardware.PWMs;
