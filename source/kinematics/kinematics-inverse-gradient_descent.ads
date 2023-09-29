--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Kinematics.Inverse.Gradient_Descent is

   procedure Solve
     (Current_Position : Kinematics.Position;
      Current_Posture  : Kinematics.Posture;
      Desired_Position : Kinematics.Position;
      Found_Position   : out Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean;
      Count            : out Natural);

end Kinematics.Inverse.Gradient_Descent;
