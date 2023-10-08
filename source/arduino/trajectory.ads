--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Kinematics;
with Reals;

package Trajectory is

   procedure Move_Swing
     (From        : Kinematics.Position;
      To          : Kinematics.Position;
      Current     : Kinematics.Posture;
      T           : Reals.Real;
      New_Posture : out Kinematics.Posture);

   procedure Move_Support
     (From        : Kinematics.Position;
      To          : Kinematics.Position;
      Current     : Kinematics.Posture;
      T           : Reals.Real;
      New_Posture : out Kinematics.Posture);

end Trajectory;
