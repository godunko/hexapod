--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This file contains formulas extracted from the Maxima.

separate (Kinematics.Forward)
procedure Compute_E_Position
  (B_x         : Reals.Real;
   B_y         : Reals.Real;
   B_z         : Reals.Real;
   Cos_Alpha_0 : Reals.Real;
   Sin_Alpha_0 : Reals.Real;
   R_1         : Reals.Real;
   Cos_Alpha_1 : Reals.Real;
   Sin_Alpha_1 : Reals.Real;
   R_2         : Reals.Real;
   Cos_Alpha_2 : Reals.Real;
   Sin_Alpha_2 : Reals.Real;
   R_3         : Reals.Real;
   Cos_Theta_1 : Reals.Real;
   Sin_Theta_1 : Reals.Real;
   Cos_Theta_2 : Reals.Real;
   Sin_Theta_2 : Reals.Real;
   Cos_Theta_3 : Reals.Real;
   Sin_Theta_3 : Reals.Real;
   X        : out Reals.Real;
   Y        : out Reals.Real;
   Z        : out Reals.Real) is
begin
   X :=
r_3*(-cos(%alpha_2)*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(-cos(%alpha_0)*cos(%alpha_1)*sin(%theta_1)-sin(%alpha_0)*cos(%alpha_1)*cos(%theta_1))*cos(%theta_2)+sin(%alpha_2)*
(cos(%alpha_0)*sin(%alpha_1)*sin(%theta_1)+sin(%alpha_0)*sin(%alpha_1)*cos(%theta_1)))*sin(%theta_3)+r_3*((-cos(%alpha_0)*cos(%alpha_1)*sin(%theta_1)-sin(%alpha_0)*cos(%alpha_1)*cos(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*cos(%theta_2))*cos(%theta_3)+
r_2*(-cos(%alpha_0)*cos(%alpha_1)*sin(%theta_1)-sin(%alpha_0)*cos(%alpha_1)*cos(%theta_1))*sin(%theta_2)+r_2*(cos(%alpha_0)*cos(%theta_1)-sin(%alpha_0)*sin(%theta_1))*cos(%theta_2)-r_1*sin(%alpha_0)*sin(%theta_1)+r_1*cos(%alpha_0)*cos(%theta_1)+b_x;

   Y :=
r_3*(-cos(%alpha_2)*(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*sin(%theta_2)+cos(%alpha_2)*(cos(%alpha_0)*cos(%alpha_1)*cos(%theta_1)-sin(%alpha_0)*cos(%alpha_1)*sin(%theta_1))*cos(%theta_2)+sin(%alpha_2)*
(sin(%alpha_0)*sin(%alpha_1)*sin(%theta_1)-cos(%alpha_0)*sin(%alpha_1)*cos(%theta_1)))*sin(%theta_3)+r_3*((cos(%alpha_0)*cos(%alpha_1)*cos(%theta_1)-sin(%alpha_0)*cos(%alpha_1)*sin(%theta_1))*sin(%theta_2)+(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*cos(%theta_2))*cos(%theta_3)+
r_2*(cos(%alpha_0)*cos(%alpha_1)*cos(%theta_1)-sin(%alpha_0)*cos(%alpha_1)*sin(%theta_1))*sin(%theta_2)+r_2*(cos(%alpha_0)*sin(%theta_1)+sin(%alpha_0)*cos(%theta_1))*cos(%theta_2)+r_1*cos(%alpha_0)*sin(%theta_1)+r_1*sin(%alpha_0)*cos(%theta_1)+b_y;

   Z :=
r_3*(sin(%alpha_1)*cos(%alpha_2)*cos(%theta_2)+cos(%alpha_1)*sin(%alpha_2))*sin(%theta_3)+r_3*sin(%alpha_1)*sin(%theta_2)*cos(%theta_3)+r_2*sin(%alpha_1)*sin(%theta_2)+b_z;
end Compute_E_Position;
