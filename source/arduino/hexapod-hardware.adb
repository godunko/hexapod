--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with BBF.Board;
with BBF.GPIO;

with Console;

package body Hexapod.Hardware is

   CHIP_FREQ_CPU_MAX : constant := 84_000_000;
   --  XXX Should be computed based on current settings of the chip

   LED : not null access BBF.GPIO.Pin'Class renames BBF.Board.Pin_LED;

   -------------------------
   -- Initialize_Hardware --
   -------------------------

   procedure Initialize_Hardware is
   begin
      --  First, turn off onboard LED

      LED.Set_Direction (BBF.GPIO.Output);
      LED.Set (False);

      --  Initialize console and output logo

      Console.Initialize (CHIP_FREQ_CPU_MAX);

      Console.New_Line;
      Console.Put_Line ("          /\ .---._");
      Console.Put_Line ("       /\/.-. /\ /\/\");
      Console.Put_Line ("     //\\oo //\\/\\\\");
      Console.Put_Line ("    //  /""/`---\\ \\""`-._");
      Console.Put_Line ("_.-'""           ""`-.`-.");
      Console.New_Line;
   end;

end Hexapod.Hardware;
