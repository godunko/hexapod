--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

package Kinematics.Jacobians.Whole_Body
  with Pure
is

   type Vector_6 is array (1 .. 6) of CGK.Reals.Real;

   type Vector_18 is array (1 .. 18) of CGK.Reals.Real;

   type Matrix_18x18 is array (1 .. 18, 1 .. 18) of CGK.Reals.Real;

   type Matrix_18x6 is array (1 .. 18, 1 .. 6) of CGK.Reals.Real;

   type Matrix_6x6 is array (1 .. 6, 1 .. 6) of CGK.Reals.Real;

   procedure Compute_Jacobian
     (LF_Posture : Kinematics.Posture;
      LM_Posture : Kinematics.Posture;
      LH_Posture : Kinematics.Posture;
      RF_Posture : Kinematics.Posture;
      RM_Posture : Kinematics.Posture;
      RH_Posture : Kinematics.Posture;
      Result     : out Matrix_18x18);

   procedure Inverse (A : Matrix_18x18; I, MA, MB : out Matrix_18x18);

   procedure JX1
     (Alpha  : CGK.Reals.Real;
      Betta  : CGK.Reals.Real;
      Gamma  : CGK.Reals.Real;
      Result : out Matrix_18x6);

   procedure JX2
     (Betta  : CGK.Reals.Real;
      Gamma  : CGK.Reals.Real;
      Result : out Matrix_6x6);

   procedure Matrix_Matrix_Product
     (Left  : Matrix_18x18;
      Right : Matrix_18x6;
      R     : out Matrix_18x6);

   procedure Matrix_Matrix_Product
     (Left  : Matrix_18x6;
      Right : Matrix_6x6;
      R     : out Matrix_18x6);

   procedure Matrix_Vector_Product
     (Left  : Matrix_18x6;
      Right : Vector_6;
      R     : out Vector_18);

   function "*" (Left : Vector_18; Right : CGK.Reals.Real) return Vector_18;

   function "-" (Right : Vector_18) return Vector_18;

end Kinematics.Jacobians.Whole_Body;
