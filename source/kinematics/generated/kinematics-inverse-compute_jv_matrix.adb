--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX - License - Identifier: Apache - 2.0
--

--  This file contains formulas extracted from the Maxima.

separate (Kinematics.Inverse)
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
   M_33        : out Reals.Real) is
begin
   M_11 :=
R_3 * (-Cos_Alpha_2 * (-Cos_Alpha_0 * Sin_Theta_1 - Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Sin_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1)) * Sin_Theta_3 + R_3 * ((Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + (-Cos_Alpha_0 * Sin_Theta_1 - Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2) * Cos_Theta_3 + R_2 * (Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + R_2 * (-Cos_Alpha_0 * Sin_Theta_1 - Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 - R_1 * Cos_Alpha_0 * Sin_Theta_1 - R_1 * Sin_Alpha_0 * Cos_Theta_1;

   M_12 :=
R_3 * (-Cos_Alpha_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Sin_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1)) * Sin_Theta_3 + R_3 * ((-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2) * Cos_Theta_3 + R_2 * (-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + R_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2 - R_1 * Sin_Alpha_0 * Sin_Theta_1 + R_1 * Cos_Alpha_0 * Cos_Theta_1;

   M_13 :=
0.0;

   M_21 :=
R_3 * (-Cos_Alpha_2 * (-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 - Cos_Alpha_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2) * Sin_Theta_3 + R_3 *
((-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 - (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2) * Cos_Theta_3 - R_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + R_2 *
(-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2;

   M_22 :=
R_3 * (-Cos_Alpha_2 * (Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 - Cos_Alpha_2 * (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2) * Sin_Theta_3 + R_3 *
((Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2 - (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2) * Cos_Theta_3 - R_2 * (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + R_2 *
(Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2;

   M_23 :=
-R_3 * Sin_Alpha_1 * Cos_Alpha_2 * Sin_Theta_2 * Sin_Theta_3 + R_3 * Sin_Alpha_1 * Cos_Theta_2 * Cos_Theta_3 + R_2 * Sin_Alpha_1 * Cos_Theta_2;

   M_31 :=
R_3 * (-Cos_Alpha_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Sin_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1)) * Cos_Theta_3 - R_3 * ((-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2) * Sin_Theta_3;

   M_32 :=
R_3 * (-Cos_Alpha_2 * (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Sin_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1 - Sin_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1)) * Cos_Theta_3 - R_3 * ((Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2) * Sin_Theta_3;

   M_33 :=
R_3 * (Sin_Alpha_1 * Cos_Alpha_2 * Cos_Theta_2 + Cos_Alpha_1 * Sin_Alpha_2) * Cos_Theta_3 - R_3 * Sin_Alpha_1 * Sin_Theta_2 * Sin_Theta_3;
end Compute_Jv_Matrix;
