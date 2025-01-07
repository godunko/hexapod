--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

package Kinematics.Jacobians.Whole_Body
  with Pure
is

   type Matrix is array (1 .. 18, 1 .. 18) of CGK.Reals.Real;

   procedure Compute_Jacobian
     (LF_Posture : Kinematics.Posture;
      LM_Posture : Kinematics.Posture;
      LH_Posture : Kinematics.Posture;
      RF_Posture : Kinematics.Posture;
      RM_Posture : Kinematics.Posture;
      RH_Posture : Kinematics.Posture;
      Result     : out Matrix);

end Kinematics.Jacobians.Whole_Body;
