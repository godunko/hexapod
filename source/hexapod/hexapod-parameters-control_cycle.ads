--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Hexapod.Parameters.Control_Cycle is

   pragma Pure;

   Tick_Frequency : constant := 100;
   Tick_Duration  : constant := 1.0 / Tick_Frequency;

end Hexapod.Parameters.Control_Cycle;
