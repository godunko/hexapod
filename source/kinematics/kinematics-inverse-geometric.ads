--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Kinematics.Inverse.Geometric is

   procedure LF_Solve
     (Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean);

   procedure RF_Solve
     (Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean);

end Kinematics.Inverse.Geometric;
