--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
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
(-cos(%alpha_2)*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)+sin(%alpha_2)*
(sin(%alpha_1)*cos(%alpha_0)*sin(%theta_1)+sin(%alpha_1)*sin(%alpha_0)*cos(%theta_1)))*sin(%theta_3)+((-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*cos(%theta_2))*cos(%theta_3);

   M_12 :=
-cos(%alpha_3)*((-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*cos(%theta_2))*sin(%theta_3)+cos(%alpha_3)*(-cos(%alpha_2)*
(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)+sin(%alpha_2)*(sin(%alpha_1)*cos(%alpha_0)*sin(%theta_1)+sin(%alpha_1)*sin(%alpha_0)*cos(%theta_1)))*
cos(%theta_3)+sin(%alpha_3)*(sin(%alpha_2)*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)-sin(%alpha_2)*(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)+cos(%alpha_2)*
(sin(%alpha_1)*cos(%alpha_0)*sin(%theta_1)+sin(%alpha_1)*sin(%alpha_0)*cos(%theta_1)));

   M_13 :=
sin(%alpha_3)*((-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*cos(%theta_2))*sin(%theta_3)-sin(%alpha_3)*(-cos(%alpha_2)*
(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)+sin(%alpha_2)*(sin(%alpha_1)*cos(%alpha_0)*sin(%theta_1)+sin(%alpha_1)*sin(%alpha_0)*cos(%theta_1)))*
cos(%theta_3)+cos(%alpha_3)*(sin(%alpha_2)*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)-sin(%alpha_2)*(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)+cos(%alpha_2)*
(sin(%alpha_1)*cos(%alpha_0)*sin(%theta_1)+sin(%alpha_1)*sin(%alpha_0)*cos(%theta_1)));

   M_14 :=
r_3*(-cos(%alpha_2)*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)+sin(%alpha_2)*
(sin(%alpha_1)*cos(%alpha_0)*sin(%theta_1)+sin(%alpha_1)*sin(%alpha_0)*cos(%theta_1)))*sin(%theta_3)+r_3*((-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*cos(%theta_2))*cos(%theta_3)+r_2*(-cos(%alpha_1)*cos(%alpha_0)*sin(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+r_2*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*cos(%theta_2)-r_1*sin(%alpha_0)*sin(%theta_1)+r_1*cos(%alpha_0)*cos(%theta_1)+b_x;

   M_21 :=
(-cos(%alpha_2)*(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*cos(%theta_2)+sin(%alpha_2)*
(sin(%alpha_1)*sin(%alpha_0)*sin(%theta_1)-sin(%alpha_1)*cos(%alpha_0)*cos(%theta_1)))*sin(%theta_3)+((cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*cos(%theta_2))*cos(%theta_3);

   M_22 :=
-cos(%alpha_3)*((cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*cos(%theta_2))*sin(%theta_3)+cos(%alpha_3)*(-cos(%alpha_2)*
(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*cos(%theta_2)+sin(%alpha_2)*(sin(%alpha_1)*sin(%alpha_0)*sin(%theta_1)-sin(%alpha_1)*cos(%alpha_0)*cos(%theta_1)))*
cos(%theta_3)+sin(%alpha_3)*(sin(%alpha_2)*(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)-sin(%alpha_2)*(cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*cos(%theta_2)+cos(%alpha_2)*
(sin(%alpha_1)*sin(%alpha_0)*sin(%theta_1)-sin(%alpha_1)*cos(%alpha_0)*cos(%theta_1)));

   M_23 :=
sin(%alpha_3)*((cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*cos(%theta_2))*sin(%theta_3)-sin(%alpha_3)*(-cos(%alpha_2)*
(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*cos(%theta_2)+sin(%alpha_2)*(sin(%alpha_1)*sin(%alpha_0)*sin(%theta_1)-sin(%alpha_1)*cos(%alpha_0)*cos(%theta_1)))*
cos(%theta_3)+cos(%alpha_3)*(sin(%alpha_2)*(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)-sin(%alpha_2)*(cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*cos(%theta_2)+cos(%alpha_2)*
(sin(%alpha_1)*sin(%alpha_0)*sin(%theta_1)-sin(%alpha_1)*cos(%alpha_0)*cos(%theta_1)));

   M_24 :=
r_3*(-cos(%alpha_2)*(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*cos(%theta_2)+sin(%alpha_2)*
(sin(%alpha_1)*sin(%alpha_0)*sin(%theta_1)-sin(%alpha_1)*cos(%alpha_0)*cos(%theta_1)))*sin(%theta_3)+r_3*((cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*cos(%theta_2))*cos(%theta_3)+
r_2*(cos(%alpha_1)*cos(%alpha_0)*cos(%theta_1)-cos(%alpha_1)*sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+r_2*(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)+r_1*cos(%alpha_0)*sin(%theta_1)+r_1*sin(%alpha_0)*cos(%theta_1)+b_y;

   M_31 :=
(sin(%alpha_1)*cos(%alpha_2)*cos(%theta_2)+cos(%alpha_1)*sin(%alpha_2))*sin(%theta_3)+sin(%alpha_1)*sin(%theta_2)*cos(%theta_3);

   M_32 :=
-sin(%alpha_1)*cos(%alpha_3)*sin(%theta_2)*sin(%theta_3)+cos(%alpha_3)*(sin(%alpha_1)*cos(%alpha_2)*cos(%theta_2)+cos(%alpha_1)*sin(%alpha_2))*cos(%theta_3)+sin(%alpha_3)*(cos(%alpha_1)*cos(%alpha_2)-sin(%alpha_1)*sin(%alpha_2)*cos(%theta_2));

   M_33 :=
sin(%alpha_1)*sin(%alpha_3)*sin(%theta_2)*sin(%theta_3)-sin(%alpha_3)*(sin(%alpha_1)*cos(%alpha_2)*cos(%theta_2)+cos(%alpha_1)*sin(%alpha_2))*cos(%theta_3)+cos(%alpha_3)*(cos(%alpha_1)*cos(%alpha_2)-sin(%alpha_1)*sin(%alpha_2)*cos(%theta_2));

   M_34 :=
r_3*(sin(%alpha_1)*cos(%alpha_2)*cos(%theta_2)+cos(%alpha_1)*sin(%alpha_2))*sin(%theta_3)+r_3*sin(%alpha_1)*sin(%theta_2)*cos(%theta_3)+r_2*sin(%alpha_1)*sin(%theta_2)+b_z;
end Compute_H_be_Matrix;
