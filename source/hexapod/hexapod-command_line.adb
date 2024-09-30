--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with A0B.Tasking;

with Hexapod.Console;
with Hexapod.Hardware;
with Hexapod.Movement;

with A0B.Types;
with Ada.Real_Time;

package body Hexapod.Command_Line is

   TCB : aliased A0B.Tasking.Task_Control_Block;

   procedure Thread;

   -------------------
   -- Register_Task --
   -------------------

   procedure Register_Task is
   begin
      A0B.Tasking.Register_Thread (TCB, Thread'Access, 16#400#);
   end Register_Task;

   ------------
   -- Thread --
   ------------

   procedure Thread is
      C : Character;

   begin
      loop
         Console.New_Line;
         Console.Put ("Phoenix Hexapod CLI> ");

         Console.Get_Synchronous (C);

         if C >= ' ' then
            Console.Put_Line ((1 => C));
         end if;


         case C is
            when 'T' | 't' =>
               declare
                  --  use type A0B.Time.Monotonic_Time;
                  use type Ada.Real_Time.Time;

                  --  Start  : constant A0B.Time.Monotonic_Time := A0B.Time.Clock;
                  --  Done   : A0B.Time.Monotonic_Time;
                  --  Cycles : A0B.Types.Unsigned_32;

                  Start : Ada.Real_Time.Time := Ada.Real_Time.Clock;
                  Done  : Ada.Real_Time.Time;
                  Span  : Ada.Real_Time.Time_Span;
                  Span_I : A0B.Types.Integer_64 with Address => Span'Address;

               begin
                  --  --  A0B.ARMv7M.DWT.DWT_CTRL.CYCCNTENA := True;
                  --  --  A0B.ARMv7M.DWT.DWT_CYCCNT.CYCCNT  := 0;
                  --
                  --  BBF.Delays.Delay_For (A0B.Time.Milliseconds (1));
                  --  --  BBF.Delays.Delay_For (A0B.Time.Microseconds (20));

                  delay until Start + Ada.Real_Time.Milliseconds (1);

                  --  Cycles := A0B.ARMv7M.DWT.DWT_CYCCNT.CYCCNT;
                  --  Done   := A0B.Time.Clock;
                  Done := Ada.Real_Time.Clock;
                  Span := Start - Done;

                  Hexapod.Console.Put_Line
                    (A0B.Types.Integer_64'Image (Span_I));
                       --  (Ada.Real_Time.To_Nanoseconds (Done - Start)));
                  --  Hexapod.Console.Put_Line
                  --    (A0B.Types.Integer_64'Image
                  --       (A0B.Time.To_Nanoseconds (Done - Start)));
                  --  Hexapod.Console.Put_Line
                  --    (A0B.Types.Unsigned_32'Image (Cycles));
               end;
            when ' ' =>
               Hexapod.Hardware.Disable_Motors_Power;

            when 'U' | 'u' =>
               Hexapod.Hardware.Enable_Motors_Power;
               Hexapod.Movement.Configure;

            --  when '-' | '_' =>
            --     if Gait /= Hexapod.Movement.Gait_Kind'First then
            --        Gait := Hexapod.Movement.Gait_Kind'Pred (@);
            --        Hexapod.Movement.Set_Gait (Gait);
            --     end if;
            --
            --  when '+' | '=' =>
            --     if Gait /= Hexapod.Movement.Gait_Kind'Last then
            --        Gait := Hexapod.Movement.Gait_Kind'Succ (@);
            --        Hexapod.Movement.Set_Gait (Gait);
            --     end if;

               --  when 'P' | 'p' =>
               --     Hexapod.Hardware.Configure_Controllers;
               --     Hexapod.Hardware.Enable_Motors_Power;
               --     Hexapod.Motor_Playground;
               --     Hexapod.Hardware.Disable_Motors_Power;

            when others =>
               null;
         end case;
      end loop;
   end Thread;

end Hexapod.Command_Line;
