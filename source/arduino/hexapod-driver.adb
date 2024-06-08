--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with A0B.ARMv7M.SysTick;
with A0B.ATSAM3X8E.TC5_Timer;
with A0B.Tasking;
with A0B.Time.Clock;
with A0B.Time.Constants;

with BBF.Board;
--  with BBF.Drivers.MPU;
--  with BBF.Drivers.MPU6050;
--  with BBF.Drivers.MPU9250;

with Reals;

with Hexapod.Command_Line;
with Hexapod.Console;
with Hexapod.Hardware;
with Hexapod.Movement;
with Hexapod.Motor_Playground;
--  with Hexapod.Motor_Power_Consumption;
with Hexapod.Remote_Control;

procedure Hexapod.Driver is

   use type A0B.Time.Monotonic_Time;
   use type A0B.Time.Time_Span;
   use type Reals.Real;
   use type Hexapod.Movement.Gait_Kind;

   Last_Timestamp : A0B.Time.Monotonic_Time :=
     A0B.Time.Constants.Monotonic_Time_First;

   procedure Body_Position_Step is
      --  Data      : BBF.Drivers.MPU9250.Sensor_Data;
      --  Data      : BBF.Drivers.MPU6050.Sensor_Data;
      --  Timestamp : BBF.Clocks.Time;

   begin
      null;
      --  Hexapod.Hardware.Body_Position_Sensor.Get (Data, Timestamp);

      --  if Timestamp - Last_Timestamp >= 1.0 then
         --  Console.Put_Line
         --    ("Body Position: GA ("
         --     & BBF.Drivers.MPU.Gravitational_Acceleration'Image
         --       (Data.Acceleration_X)
         --     & " "
         --     & BBF.Drivers.MPU.Gravitational_Acceleration'Image
         --       (Data.Acceleration_Y)
         --     & " "
         --     & BBF.Drivers.MPU.Gravitational_Acceleration'Image
         --       (Data.Acceleration_Z)
         --     & ")  AV ("
         --     & BBF.Drivers.MPU.Angular_Velosity'Image (Data.Velocity_U)
         --     & " "
         --     & BBF.Drivers.MPU.Angular_Velosity'Image (Data.Velocity_U)
         --     & " "
         --     & BBF.Drivers.MPU.Angular_Velosity'Image (Data.Velocity_U)
         --     & ")");
         --  Last_Timestamp := Timestamp;
      --  end if;
   end Body_Position_Step;

   --  Tick_Duration    : constant A0B.Time.Time_Span :=
   --    A0B.Time.Milliseconds (1000 / Hexapod.Movement.Ticks);
   --  Movement_Enabled : Boolean := False;
   --  Next_Tick        : A0B.Time.Monotonic_Time;
   --  Gait             : Hexapod.Movement.Gait_Kind := Hexapod.Movement.Stop;
   --
   --  C                : Character;

begin
   A0B.ARMv7M.SysTick.Initialize
    (Use_Processor_Clock => True,
     Clock_Frequency     => 84_000_000);
   A0B.ATSAM3X8E.TC5_Timer.Initialize
     (Master_Clock_Frequency => 84_000_000,
      Source                 => A0B.ATSAM3X8E.TC5_Timer.MCK_32);

   Hexapod.Hardware.Initialize_Hardware;
   --  Hexapod.Motor_Power_Consumption.Initialize;
   Hexapod.Movement.Initialize;

   A0B.Tasking.Initialize (16#400#);

   Hexapod.Command_Line.Register_Task;
   Hexapod.Remote_Control.Register_Task;
   Hexapod.Movement.Register_Task;

   A0B.Tasking.Run;

   --  Next_Tick := A0B.Time.Clock + Tick_Duration;
   --
   --  loop
   --     Console.New_Line;
   --     Console.Put ("Phoenix Hexapod CLI> ");
   --
   --     loop
   --        declare
   --           Success : Boolean := True;
   --
   --        begin
   --           Console.Get_Asynchronous (C, Success);
   --
   --           exit when Success;
   --        end;
   --
   --        if Next_Tick <= A0B.Time.Clock then
   --           if A0B.Time.Clock - Next_Tick > Tick_Duration then
   --              Console.Put ("-");
   --           end if;
   --
   --           Next_Tick := A0B.Time.Clock + Tick_Duration;
   --
   --           if Movement_Enabled then
   --              Hexapod.Movement.Step;
   --           end if;
   --
   --           --  Hexapod.Motor_Power_Consumption.Step;
   --           Body_Position_Step;
   --        end if;
   --     end loop;
   --  end loop;
end Hexapod.Driver;
