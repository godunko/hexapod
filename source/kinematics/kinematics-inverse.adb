--
--  Copyright (C) 2023-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Kinematics.Configuration.Derived;
with Reals;

package body Kinematics.Inverse is

   use type Reals.Real;

   procedure Compute_Jv_Matrix
     (Cos_Alpha_0 : Reals.Real;
      Sin_Alpha_0 : Reals.Real;
      Cos_Alpha_1 : Reals.Real;
      Sin_Alpha_1 : Reals.Real;
      Cos_Alpha_2 : Reals.Real;
      Sin_Alpha_2 : Reals.Real;
      Cos_Theta_1 : Reals.Real;
      Sin_Theta_1 : Reals.Real;
      Cos_Theta_2 : Reals.Real;
      Sin_Theta_2 : Reals.Real;
      Cos_Theta_3 : Reals.Real;
      Sin_Theta_3 : Reals.Real;
      R_1         : Reals.Real;
      R_2         : Reals.Real;
      R_3         : Reals.Real;
      M_11        : out Reals.Real;
      M_12        : out Reals.Real;
      M_13        : out Reals.Real;
      M_21        : out Reals.Real;
      M_22        : out Reals.Real;
      M_23        : out Reals.Real;
      M_31        : out Reals.Real;
      M_32        : out Reals.Real;
      M_33        : out Reals.Real) is separate;

   -----------------
   -- LF_Jacobian --
   -----------------

   procedure LF_Jacobian
     (Posture : Kinematics.Posture;
      Result  : out Jacobian_Matrix)
   is
      Cos_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_1);
      Sin_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_1);
      Cos_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_2);
      Sin_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_2);
      Cos_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_3);
      Sin_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_3);

   begin
      Compute_Jv_Matrix
        (Cos_Alpha_0 => Kinematics.Configuration.Derived.LF_Cos_Gamma_0,
         Sin_Alpha_0 => Kinematics.Configuration.Derived.LF_Sin_Gamma_0,
         Cos_Alpha_1 => Kinematics.Configuration.Derived.LF_Cos_Alpha_1,
         Sin_Alpha_1 => Kinematics.Configuration.Derived.LF_Sin_Alpha_1,
         Cos_Alpha_2 => Kinematics.Configuration.Derived.LF_Cos_Alpha_2,
         Sin_Alpha_2 => Kinematics.Configuration.Derived.LF_Sin_Alpha_2,
         Cos_Theta_1 => Cos_Theta_1,
         Sin_Theta_1 => Sin_Theta_1,
         Cos_Theta_2 => Cos_Theta_2,
         Sin_Theta_2 => Sin_Theta_2,
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         R_1         => Kinematics.Configuration.LF_DH_R1,
         R_2         => Kinematics.Configuration.LF_DH_R2,
         R_3         => Kinematics.Configuration.LF_DH_R3,
         M_11        => Result.Matrix.M_11,
         M_12        => Result.Matrix.M_12,
         M_13        => Result.Matrix.M_13,
         M_21        => Result.Matrix.M_21,
         M_22        => Result.Matrix.M_22,
         M_23        => Result.Matrix.M_23,
         M_31        => Result.Matrix.M_31,
         M_32        => Result.Matrix.M_32,
         M_33        => Result.Matrix.M_33);
   end LF_Jacobian;

end Kinematics.Inverse;
