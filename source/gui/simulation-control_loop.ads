--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Control loop to simulate robot's movement.

with GUI.Scene_States;

package Simulation.Control_Loop is

   procedure Initialize;

   procedure Start;

   procedure Finalize;

   function Get_Scene return GUI.Scene_States.Scene_Information;

end Simulation.Control_Loop;
