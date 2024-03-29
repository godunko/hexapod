--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with BBF.Board;
with BBF.Drivers.MPU;
--  with BBF.Drivers.MPU6050;
with BBF.Drivers.MPU9250;
with BBF.Clocks;

with Reals;

with Hexapod.Console;
with Hexapod.Hardware;
with Hexapod.Movement;
with Hexapod.Motor_Playground;
with Hexapod.Motor_Power_Consumption;

procedure Hexapod.Driver is

   use type Reals.Real;
   use type BBF.Clocks.Time;
   use type Hexapod.Movement.Gait_Kind;

   Last_Timestamp : BBF.Clocks.Time := 0.0;

   procedure Body_Position_Step is
      Data      : BBF.Drivers.MPU9250.Sensor_Data;
      --  Data      : BBF.Drivers.MPU6050.Sensor_Data;
      Timestamp : BBF.Clocks.Time;

   begin
      Hexapod.Hardware.Body_Position_Sensor.Get (Data, Timestamp);

      if Timestamp - Last_Timestamp >= 1.0 then
         Console.Put_Line
           ("Body Position: GA ("
            & BBF.Drivers.MPU.Gravitational_Acceleration'Image
              (Data.Acceleration_X)
            & " "
            & BBF.Drivers.MPU.Gravitational_Acceleration'Image
              (Data.Acceleration_Y)
            & " "
            & BBF.Drivers.MPU.Gravitational_Acceleration'Image
              (Data.Acceleration_Z)
            & ")  AV ("
            & BBF.Drivers.MPU.Angular_Velosity'Image (Data.Velocity_U)
            & " "
            & BBF.Drivers.MPU.Angular_Velosity'Image (Data.Velocity_U)
            & " "
            & BBF.Drivers.MPU.Angular_Velosity'Image (Data.Velocity_U)
            & ")");
         Last_Timestamp := Timestamp;
      end if;
   end Body_Position_Step;

   Tick_Duration    : constant := 1.0 / Hexapod.Movement.Ticks;
   Movement_Enabled : Boolean := False;
   Next_Tick        : BBF.Clocks.Time;
   Gait             : Hexapod.Movement.Gait_Kind := Hexapod.Movement.Stop;

   C                : Character;

begin
   Hexapod.Hardware.Initialize_Hardware;
   Hexapod.Motor_Power_Consumption.Initialize;
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

            Hexapod.Motor_Power_Consumption.Step;
            Body_Position_Step;
         end if;
      end loop;

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
            Movement_Enabled := not @;

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

         when 'P' | 'p' =>
            Hexapod.Hardware.Configure_Controllers;
            Hexapod.Hardware.Enable_Motors_Power;
            Hexapod.Motor_Playground;
            Hexapod.Hardware.Disable_Motors_Power;

         when others =>
            null;
      end case;
   end loop;
end Hexapod.Driver;
