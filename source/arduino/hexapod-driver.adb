--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Console;

procedure Hexapod.Driver is
   CHIP_FREQ_CPU_MAX : constant := 84_000_000;
   --  XXX Should be computed based on current settings of the chip

begin
   Console.Initialize (CHIP_FREQ_CPU_MAX);

   Console.New_Line;
   Console.Put_Line ("          /\ .---._");
   Console.Put_Line ("       /\/.-. /\ /\/\");
   Console.Put_Line ("     //\\oo //\\/\\\\");
   Console.Put_Line ("    //  /""/`---\\ \\""`-._");
   Console.Put_Line ("_.-'""           ""`-.`-.");
   Console.New_Line;

   Console.Put_Line ("Phoenix Hexapod");
end Hexapod.Driver;
