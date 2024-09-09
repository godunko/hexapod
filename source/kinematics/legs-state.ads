--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

package Legs.State
  with Preelaborate
is

   Position : array (Leg_Index) of Kinematics.Position;
   Posture  : array (Leg_Index) of Kinematics.Posture;

end Legs.State;
