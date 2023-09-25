--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
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
r_3*(-cos(%alpha_2)*(-cos(%alpha_0)*sin(%theta_1)-sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1))*cos(%theta_2)+sin(%alpha_2)*
(sin(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-sin(%alpha_1)*sin(%alpha_0)*sin(%theta_1)))*sin(%theta_3)+r_3*((cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1))*sin(%theta_2)+(-cos(%alpha_0)*sin(%theta_1)-sin(%alpha_0)*cos(%theta_1))*cos(%theta_2))*cos(%theta_3)+r_2*(cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1))*sin(%theta_2)+r_2*(-cos(%alpha_0)*sin(%theta_1)-sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)-r_1*cos(%alpha_0)*sin(%theta_1)-r_1*sin(%alpha_0)*cos(%theta_1);

   M_12 :=
r_3*(-cos(%alpha_2)*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)+sin(%alpha_2)*
(sin(%alpha_1)*cos(%alpha_0)*sin(%theta_1)+sin(%alpha_1)*sin(%alpha_0)*cos(%theta_1)))*sin(%theta_3)+r_3*((-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*cos(%theta_2))*cos(%theta_3)+r_2*(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+r_2*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*cos(%theta_2)-r_1*sin(%alpha_0)*sin(%theta_1)+r_1*cos(%alpha_0)*cos(%theta_1);

   M_13 :=
0.0;

   M_21 :=
r_3*(-cos(%alpha_2)*(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)-cos(%alpha_2)*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*cos(%theta_2))*sin(%theta_3)+r_3*
((-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)-(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*sin(%theta_2))*cos(%theta_3)-r_2*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+r_2*
(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*cos(%theta_2);

   M_22 :=
r_3*(-cos(%alpha_2)*(cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)-cos(%alpha_2)*(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*cos(%theta_2))*sin(%theta_3)+r_3*
((cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*cos(%theta_2)-(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*sin(%theta_2))*cos(%theta_3)-r_2*(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+r_2*
(cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*cos(%theta_2);

   M_23 :=
-r_3*sin(%alpha_1)*cos(%alpha_2)*sin(%theta_2)*sin(%theta_3)+r_3*sin(%alpha_1)*cos(%theta_2)*cos(%theta_3)+r_2*sin(%alpha_1)*cos(%theta_2);

   M_31 :=
r_3*(-cos(%alpha_2)*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)+sin(%alpha_2)*
(sin(%alpha_1)*cos(%alpha_0)*sin(%theta_1)+sin(%alpha_1)*sin(%alpha_0)*cos(%theta_1)))*cos(%theta_3)-r_3*((-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*cos(%theta_2))*sin(%theta_3);

   M_32 :=
r_3*(-cos(%alpha_2)*(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*cos(%theta_2)+sin(%alpha_2)*
(sin(%alpha_1)*sin(%alpha_0)*sin(%theta_1)-sin(%alpha_1)*cos(%alpha_0)*cos(%theta_1)))*cos(%theta_3)-r_3*((cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*cos(%theta_2))*sin(%theta_3);

   M_33 :=
r_3*(sin(%alpha_1)*cos(%alpha_2)*cos(%theta_2)+cos(%alpha_1)*sin(%alpha_2))*cos(%theta_3)-r_3*sin(%alpha_1)*sin(%theta_2)*sin(%theta_3);
end Compute_Jv_Matrix;
