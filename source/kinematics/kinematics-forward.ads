--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Computes basic matricies of Forward Kinematics. Configuration parameters
--  are defined in the Kinematics.Configuration package.

with Reals;

package Kinematics.Forward is

   --  pragma Pure;

   procedure LF_T_BE
     (Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real;
      Result  : out Reals.Transformations_3D.Transformation_3D);
   --  Create transformation to compute position of the end effector.

end Kinematics.Forward;
