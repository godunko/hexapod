--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with A0B.ARMv7M.SysTick;
with A0B.STM32F401.TIM11_Timer;
with A0B.Tasking;

with Debug.Log.Console;
with Hexapod.Command_Line;
with Hexapod.Hardware;
with Hexapod.Movement;
--  with Hexapod.Motor_Power_Consumption;
with Hexapod.Remote_Control;

with Hexapod.Console;

procedure Hexapod.Driver is
begin
   A0B.ARMv7M.SysTick.Initialize
    (Use_Processor_Clock => True,
     Clock_Frequency     => 84_000_000);
   A0B.STM32F401.TIM11_Timer.Initialize
     (Timer_Peripheral_Frequency => 84_000_000);

   Hexapod.Console.Initialize;
   Debug.Log.Console.Initialize;

   Hexapod.Hardware.Initialize_Hardware;
   Hexapod.Remote_Control.Initialize;
   --  Hexapod.Motor_Power_Consumption.Initialize;
   Hexapod.Movement.Initialize;

   A0B.Tasking.Initialize (16#400#);

   Hexapod.Command_Line.Register_Task;
   Hexapod.Remote_Control.Register_Task;
   Hexapod.Movement.Register_Task;

   A0B.Tasking.Run;
end Hexapod.Driver;
