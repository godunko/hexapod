--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with A0B.Tasking;

with Hexapod.Console;
with Hexapod.Hardware;
with Hexapod.Movement;

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
      use type Hexapod.Movement.Gait_Kind;

      C    : Character;
      Gait : Hexapod.Movement.Gait_Kind := Hexapod.Movement.Stop;

   begin
      loop
         Console.New_Line;
         Console.Put ("Phoenix Hexapod CLI> ");

         Console.Get_Synchronous (C);

         if C >= ' ' then
            Console.Put_Line ((1 => C));
         end if;

         case C is
            when ' ' =>
               Hexapod.Hardware.Disable_Motors_Power;

            when 'U' | 'u' =>
               Hexapod.Hardware.Configure_Controllers;
               Hexapod.Hardware.Enable_Motors_Power;
               Hexapod.Movement.Prepare;

            when 'M' | 'm' =>
               Hexapod.Movement.Movement_Enabled := not @;

            when '-' | '_' =>
               if Gait /= Hexapod.Movement.Gait_Kind'First then
                  Gait := Hexapod.Movement.Gait_Kind'Pred (@);
                  Hexapod.Movement.Set_Gait (Gait);
               end if;

            when '+' | '=' =>
               if Gait /= Hexapod.Movement.Gait_Kind'Last then
                  Gait := Hexapod.Movement.Gait_Kind'Succ (@);
                  Hexapod.Movement.Set_Gait (Gait);
               end if;

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
