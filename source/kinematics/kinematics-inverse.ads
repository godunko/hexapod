--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Reals;

package Kinematics.Inverse is

   pragma Pure;

   type Jacobian_Matrix is private;
   --  Matrix to store Jacobian. It is limited to linear velocities components
   --  due to limitations of the 3DoF hexapod capabilities.

   procedure LF_Jacobian
     (Posture : Kinematics.Posture;
      Result  : out Jacobian_Matrix);

private

   type Jacobian_Matrix is record
      Matrix : Reals.Matricies_3x3.Matrix_3x3;
   end record;

end Kinematics.Inverse;
