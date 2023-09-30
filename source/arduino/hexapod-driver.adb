--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Console;
with Hexapod.Hardware;

procedure Hexapod.Driver is
begin
   Hexapod.Hardware.Initialize_Hardware;
   Console.Put_Line ("Phoenix Hexapod");
end Hexapod.Driver;
