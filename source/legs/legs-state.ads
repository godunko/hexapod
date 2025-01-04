--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  pragma Restrictions (No_Elaboration_Code);

package Legs.State
  with Preelaborate
is

   Legs : array (Leg_Index) of Leg;

   procedure Initialize;

end Legs.State;
