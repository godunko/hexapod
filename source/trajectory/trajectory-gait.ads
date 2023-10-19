--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Computation of position of all legs based on gait description.

with Kinematics;
with Reals;

package Trajectory.Gait is

   type Gait_Descriptor is record
      LF_Fase     : Reals.Real;
      LM_Fase     : Reals.Real;
      LH_Fase     : Reals.Real;
      RF_Fase     : Reals.Real;
      RM_Fase     : Reals.Real;
      RH_Fase     : Reals.Real;
      Duty_Factor : Reals.Real;
   end record;

   procedure Position
     (Descriptor : Gait_Descriptor;
      LF_Base    : Kinematics.Position;
      LM_Base    : Kinematics.Position;
      LH_Base    : Kinematics.Position;
      RF_Base    : Kinematics.Position;
      RM_Base    : Kinematics.Position;
      RH_Base    : Kinematics.Position;
      Cycle      : Reals.Real;
      Time       : Reals.Real;
      Length_X   : Reals.Real;
      Length_Y   : Reals.Real;
      Height_Z   : Reals.Real;
      LF_Position : out Kinematics.Position;
      LM_Position : out Kinematics.Position;
      LH_Position : out Kinematics.Position;
      RF_Position : out Kinematics.Position;
      RM_Position : out Kinematics.Position;
      RH_Position : out Kinematics.Position);

end Trajectory.Gait;
