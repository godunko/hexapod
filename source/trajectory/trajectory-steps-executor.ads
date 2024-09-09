--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Kinematics;

package Trajectory.Steps.Executor is

   pragma Pure;

   procedure Compute_Position
     (LF_Base     : Kinematics.Position;
      LM_Base     : Kinematics.Position;
      LH_Base     : Kinematics.Position;
      RF_Base     : Kinematics.Position;
      RM_Base     : Kinematics.Position;
      RH_Base     : Kinematics.Position;
      Plan        : Step_Plan_Descriptor;
      Fase        : Step_Fase;
      LF_Position : in out Kinematics.Position;
      LM_Position : in out Kinematics.Position;
      LH_Position : in out Kinematics.Position;
      RF_Position : in out Kinematics.Position;
      RM_Position : in out Kinematics.Position;
      RH_Position : in out Kinematics.Position);

end Trajectory.Steps.Executor;

