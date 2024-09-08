--
--  Copyright (C) 2023-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Kinematics.Inverse.Geometric is

   pragma Pure;

   procedure Solve
     (B_X              : Reals.Real;
      B_Y              : Reals.Real;
      B_Z              : Reals.Real;
      Cos_Gamma_0      : Reals.Real;
      Sin_Gamma_0      : Reals.Real;
      R_1              : Reals.Real;
      R_2              : Reals.Real;
      R_3              : Reals.Real;
      Inverse          : Boolean;
      Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean);
   --  Compute leg posture from the desired position of the end effector.
   --
   --  @param Inverse Whether to inverse values of the second and third
   --  components of the posture. It might be necessary due to mirror
   --  orientation of the motors on side of the robot.

end Kinematics.Inverse.Geometric;
