--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX - License - Identifier: Apache - 2.0
--

--  This file contains formulas extracted from the Maxima.

separate (Kinematics.Forward)
procedure Compute_H_be_Matrix
  (Cos_Alpha_0 : Reals.Real;
   Sin_Alpha_0 : Reals.Real;
   Cos_Alpha_1 : Reals.Real;
   Sin_Alpha_1 : Reals.Real;
   Cos_Alpha_2 : Reals.Real;
   Sin_Alpha_2 : Reals.Real;
   Cos_Alpha_3 : Reals.Real;
   Sin_Alpha_3 : Reals.Real;
   Cos_Theta_1 : Reals.Real;
   Sin_Theta_1 : Reals.Real;
   Cos_Theta_2 : Reals.Real;
   Sin_Theta_2 : Reals.Real;
   Cos_Theta_3 : Reals.Real;
   Sin_Theta_3 : Reals.Real;
   R_1         : Reals.Real;
   R_2         : Reals.Real;
   R_3         : Reals.Real;
   B_x         : Reals.Real;
   B_y         : Reals.Real;
   B_z         : Reals.Real;
   M_11        : out Reals.Real;
   M_12        : out Reals.Real;
   M_13        : out Reals.Real;
   M_14        : out Reals.Real;
   M_21        : out Reals.Real;
   M_22        : out Reals.Real;
   M_23        : out Reals.Real;
   M_24        : out Reals.Real;
   M_31        : out Reals.Real;
   M_32        : out Reals.Real;
   M_33        : out Reals.Real;
   M_34        : out Reals.Real) is
begin
   M_11 :=
(-Cos_Alpha_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Sin_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1)) * Sin_Theta_3 + ((-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2) * Cos_Theta_3;

   M_12 :=
-Cos_Alpha_3 * ((-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2) * Sin_Theta_3 + Cos_Alpha_3 * (-Cos_Alpha_2 *
(Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 * (Sin_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1)) *
Cos_Theta_3 + Sin_Alpha_3 * (Sin_Alpha_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 - Sin_Alpha_2 * (-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 + Cos_Alpha_2 *
(Sin_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1));

   M_13 :=
Sin_Alpha_3 * ((-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2) * Sin_Theta_3 - Sin_Alpha_3 * (-Cos_Alpha_2 *
(Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 * (Sin_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1)) *
Cos_Theta_3 + Cos_Alpha_3 * (Sin_Alpha_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 - Sin_Alpha_2 * (-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 + Cos_Alpha_2 *
(Sin_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1));

   M_14 :=
R_3 * (-Cos_Alpha_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Sin_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1)) * Sin_Theta_3 + R_3 * ((-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2) * Cos_Theta_3 + R_2 * (-Cos_Alpha_1 * Cos_Alpha_0 * Sin_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + R_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2 - R_1 * Sin_Alpha_0 * Sin_Theta_1 + R_1 * Cos_Alpha_0 * Cos_Theta_1 + B_x;

   M_21 :=
(-Cos_Alpha_2 * (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Sin_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1 - Sin_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1)) * Sin_Theta_3 + ((Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2) * Cos_Theta_3;

   M_22 :=
-Cos_Alpha_3 * ((Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2) * Sin_Theta_3 + Cos_Alpha_3 * (-Cos_Alpha_2 *
(Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 * (Sin_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1 - Sin_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1)) *
Cos_Theta_3 + Sin_Alpha_3 * (Sin_Alpha_2 * (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 - Sin_Alpha_2 * (Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2 + Cos_Alpha_2 *
(Sin_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1 - Sin_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1));

   M_23 :=
Sin_Alpha_3 * ((Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2) * Sin_Theta_3 - Sin_Alpha_3 * (-Cos_Alpha_2 *
(Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 * (Sin_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1 - Sin_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1)) *
Cos_Theta_3 + Cos_Alpha_3 * (Sin_Alpha_2 * (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 - Sin_Alpha_2 * (Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2 + Cos_Alpha_2 *
(Sin_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1 - Sin_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1));

   M_24 :=
R_3 * (-Cos_Alpha_2 * (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Sin_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1 - Sin_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1)) * Sin_Theta_3 + R_3 * ((Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2) * Cos_Theta_3 + 
R_2 * (Cos_Alpha_1 * Cos_Alpha_0 * Cos_Theta_1 - Cos_Alpha_1 * Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + R_2 * (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 + R_1 * Cos_Alpha_0 * Sin_Theta_1 + R_1 * Sin_Alpha_0 * Cos_Theta_1 + B_y;

   M_31 :=
(Sin_Alpha_1 * Cos_Alpha_2 * Cos_Theta_2 + Cos_Alpha_1 * Sin_Alpha_2) * Sin_Theta_3 + Sin_Alpha_1 * Sin_Theta_2 * Cos_Theta_3;

   M_32 :=
-Sin_Alpha_1 * Cos_Alpha_3 * Sin_Theta_2 * Sin_Theta_3 + Cos_Alpha_3 * (Sin_Alpha_1 * Cos_Alpha_2 * Cos_Theta_2 + Cos_Alpha_1 * Sin_Alpha_2) * Cos_Theta_3 + Sin_Alpha_3 * (Cos_Alpha_1 * Cos_Alpha_2 - Sin_Alpha_1 * Sin_Alpha_2 * Cos_Theta_2);

   M_33 :=
Sin_Alpha_1 * Sin_Alpha_3 * Sin_Theta_2 * Sin_Theta_3 - Sin_Alpha_3 * (Sin_Alpha_1 * Cos_Alpha_2 * Cos_Theta_2 + Cos_Alpha_1 * Sin_Alpha_2) * Cos_Theta_3 + Cos_Alpha_3 * (Cos_Alpha_1 * Cos_Alpha_2 - Sin_Alpha_1 * Sin_Alpha_2 * Cos_Theta_2);

   M_34 :=
R_3 * (Sin_Alpha_1 * Cos_Alpha_2 * Cos_Theta_2 + Cos_Alpha_1 * Sin_Alpha_2) * Sin_Theta_3 + R_3 * Sin_Alpha_1 * Sin_Theta_2 * Cos_Theta_3 + R_2 * Sin_Alpha_1 * Sin_Theta_2 + B_z;
end Compute_H_be_Matrix;
