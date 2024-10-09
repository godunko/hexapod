--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with A0B.MPUXXXX;

with BBF.Awaits;

with Hexapod.Console;

package body Hexapod.Hardware.IMUs is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Success : Boolean := True;

   begin
      declare
         Await : aliased BBF.Awaits.Await;

      begin
         IMUB.Initialize
           (Finished => BBF.Awaits.Create_Callback (Await),
            Success     => Success);

         if Success then
            BBF.Awaits.Suspend_Till_Callback (Await);
         end if;

         if not Success then
            Hexapod.Console.Put_Line ("IMUB: initialization failure");
         end if;
      end;

      declare
         Await : aliased BBF.Awaits.Await;

      begin
         IMUB.Configure
           --  (FIFO_Rate => 100,
           --   Finished  => BBF.Awaits.Create_Callback (Await),
           --   Success   => Success);
           (Accelerometer_Range => A0B.MPUXXXX.FSR_2G,
            Gyroscope_Range     => A0B.MPUXXXX.FSR_2000DPS,
            Temperature         => True,
            Filter              => True,
            Sample_Rate         => 198,
            Finished            => BBF.Awaits.Create_Callback (Await),
            Success             => Success);

         if Success then
            BBF.Awaits.Suspend_Till_Callback (Await);
         end if;

         if not Success then
            Hexapod.Console.Put_Line ("IMUB: configuration failure");
         end if;
      end;

      declare
         Await : aliased BBF.Awaits.Await;

      begin
         IMUB.Enable
           (Finished => BBF.Awaits.Create_Callback (Await),
            Success  => Success);

         if Success then
            BBF.Awaits.Suspend_Till_Callback (Await);
         end if;

         if not Success then
            Hexapod.Console.Put_Line ("IMUB: activation failure");
         end if;
      end;

      if Success then
         Hexapod.Console.Put_Line ("IMUB: initialized, configured and active");
      end if;
   end Initialize;

end Hexapod.Hardware.IMUs;
