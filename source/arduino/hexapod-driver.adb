--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with BBF.Board;
with BBF.Clocks;

with Reals;

with Hexapod.Console;
with Hexapod.Hardware;
with Hexapod.Movement;

procedure Hexapod.Driver is

   use type Reals.Real;
   use type BBF.Clocks.Time;

   Tick_Duration    : constant := 1.0 / Hexapod.Movement.Ticks;
   Movement_Enabled : Boolean := False;
   Next_Tick        : BBF.Clocks.Time;

   Step_Length_X    : Reals.Real := 0.0;
   Step_Length_Y    : Reals.Real := 0.0;

   C                : Character;

begin
   Hexapod.Hardware.Initialize_Hardware;
   Hexapod.Movement.Initialize;

   Next_Tick := BBF.Board.Real_Time_Clock_Controller.Clock + Tick_Duration;

   loop
      Console.New_Line;
      Console.Put ("Phoenix Hexapod CLI> ");

      loop
         declare
            Success : Boolean := True;

         begin
            Console.Get_Asynchronous (C, Success);

            exit when Success;
         end;

         if Next_Tick <= BBF.Board.Real_Time_Clock_Controller.Clock then
            if BBF.Board.Real_Time_Clock_Controller.Clock - Next_Tick
                 > Tick_Duration
            then
               Console.Put ("-");
            end if;

            Next_Tick :=
              BBF.Board.Real_Time_Clock_Controller.Clock + Tick_Duration;

            if Movement_Enabled then
               Hexapod.Movement.Step;
            end if;
         end if;
      end loop;

      if C >= ' ' then
         Console.Put_Line ((1 => C));
      end if;

      case C is
         when 'U' | 'u' =>
            Hexapod.Hardware.Configure_Controllers;
            Hexapod.Movement.Set_Step_Length (Step_Length_X, Step_Length_Y);
            Hexapod.Movement.Prepare;

         when 'W' | 'w' =>
            Step_Length_X := @ + 0.010;
            Hexapod.Movement.Set_Step_Length (Step_Length_X, Step_Length_Y);

         when 'S' | 's' =>
            Step_Length_X := @ - 0.010;
            Hexapod.Movement.Set_Step_Length (Step_Length_X, Step_Length_Y);

         when 'D' | 'd' =>
            Step_Length_Y := @ + 0.010;
            Hexapod.Movement.Set_Step_Length (Step_Length_X, Step_Length_Y);

         when 'A' | 'a' =>
            Step_Length_Y := @ - 0.010;
            Hexapod.Movement.Set_Step_Length (Step_Length_X, Step_Length_Y);

         --  when 'R' | 'r' =>
         --     Move (0.000, 0.000, 0.005);
         --
         --  when 'F' | 'f' =>
         --     Move (0.000, 0.000, -0.005);

         when 'M' | 'm' =>
            Movement_Enabled := not @;

         when others =>
            null;
      end case;
   end loop;
end Hexapod.Driver;
