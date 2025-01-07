--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This file is create from the formulas in `kinematics-jacobian.wxmx` file

procedure Kinematics.Generic_Linear_Velocity_Jacobian
  (Cos_Theta_1 : CGK.Reals.Real;
   Sin_Theta_1 : CGK.Reals.Real;
   Cos_Theta_2 : CGK.Reals.Real;
   Sin_Theta_2 : CGK.Reals.Real;
   Cos_Theta_3 : CGK.Reals.Real;
   Sin_Theta_3 : CGK.Reals.Real;
   M_11        : out CGK.Reals.Real;
   M_12        : out CGK.Reals.Real;
   M_13        : out CGK.Reals.Real;
   M_21        : out CGK.Reals.Real;
   M_22        : out CGK.Reals.Real;
   M_23        : out CGK.Reals.Real;
   M_31        : out CGK.Reals.Real;
   M_32        : out CGK.Reals.Real;
   M_33        : out CGK.Reals.Real)
is
   use type CGK.Reals.Real;

begin
   M_11 :=
r_3*(-cos(α_2)*(-cos(b_γ)*sin(θ_1)-sin(b_γ)*cos(θ_1))*sin(θ_2)+cos(α_2)*(sin(b_γ)*cos(α_1)*sin(θ_1)-cos(b_γ)*cos(α_1)*cos(θ_1))*cos(θ_2)+sin(α_2)*
(cos(b_γ)*sin(α_1)*cos(θ_1)-sin(b_γ)*sin(α_1)*sin(θ_1)))*sin(θ_3)+r_3*
((sin(b_γ)*cos(α_1)*sin(θ_1)-cos(b_γ)*cos(α_1)*cos(θ_1))*sin(θ_2)+(-cos(b_γ)*sin(θ_1)-sin(b_γ)*cos(θ_1))*cos(θ_2))*cos(θ_3)+d_3*(sin(α_2)*(-cos(b_γ)*sin(θ_1)-sin(b_γ)*cos(θ_1))*
sin(θ_2)-sin(α_2)*(sin(b_γ)*cos(α_1)*sin(θ_1)-cos(b_γ)*cos(α_1)*cos(θ_1))*cos(θ_2)+cos(α_2)*(cos(b_γ)*sin(α_1)*cos(θ_1)-sin(b_γ)*sin(α_1)*sin(θ_1)))+r_2*
(sin(b_γ)*cos(α_1)*sin(θ_1)-cos(b_γ)*cos(α_1)*cos(θ_1))*sin(θ_2)+r_2*(-cos(b_γ)*sin(θ_1)-sin(b_γ)*cos(θ_1))*cos(θ_2)+d_2*(cos(b_γ)*sin(α_1)*cos(θ_1)-sin(b_γ)*sin(α_1)*sin(θ_1))-
r_1*cos(b_γ)*sin(θ_1)-r_1*sin(b_γ)*cos(θ_1);
   M_21 :=
r_3*(-cos(α_2)*(cos(b_γ)*cos(θ_1)-sin(b_γ)*sin(θ_1))*sin(θ_2)+cos(α_2)*(-cos(b_γ)*cos(α_1)*sin(θ_1)-sin(b_γ)*cos(α_1)*cos(θ_1))*cos(θ_2)+sin(α_2)*
(cos(b_γ)*sin(α_1)*sin(θ_1)+sin(b_γ)*sin(α_1)*cos(θ_1)))*sin(θ_3)+r_3*
((-cos(b_γ)*cos(α_1)*sin(θ_1)-sin(b_γ)*cos(α_1)*cos(θ_1))*sin(θ_2)+(cos(b_γ)*cos(θ_1)-sin(b_γ)*sin(θ_1))*cos(θ_2))*cos(θ_3)+d_3*(sin(α_2)*(cos(b_γ)*cos(θ_1)-sin(b_γ)*sin(θ_1))*
sin(θ_2)-sin(α_2)*(-cos(b_γ)*cos(α_1)*sin(θ_1)-sin(b_γ)*cos(α_1)*cos(θ_1))*cos(θ_2)+cos(α_2)*(cos(b_γ)*sin(α_1)*sin(θ_1)+sin(b_γ)*sin(α_1)*cos(θ_1)))+r_2*
(-cos(b_γ)*cos(α_1)*sin(θ_1)-sin(b_γ)*cos(α_1)*cos(θ_1))*sin(θ_2)+r_2*(cos(b_γ)*cos(θ_1)-sin(b_γ)*sin(θ_1))*cos(θ_2)+d_2*(cos(b_γ)*sin(α_1)*sin(θ_1)+sin(b_γ)*sin(α_1)*cos(θ_1))-
r_1*sin(b_γ)*sin(θ_1)+r_1*cos(b_γ)*cos(θ_1);
   M_31 := 0.0;

   M_12 :=
r_3*(-cos(α_2)*(-cos(b_γ)*cos(α_1)*sin(θ_1)-sin(b_γ)*cos(α_1)*cos(θ_1))*sin(θ_2)-cos(α_2)*(cos(b_γ)*cos(θ_1)-sin(b_γ)*sin(θ_1))*cos(θ_2))*sin(θ_3)+r_3*
((-cos(b_γ)*cos(α_1)*sin(θ_1)-sin(b_γ)*cos(α_1)*cos(θ_1))*cos(θ_2)-(cos(b_γ)*cos(θ_1)-sin(b_γ)*sin(θ_1))*sin(θ_2))*cos(θ_3)+d_3*
(sin(α_2)*(-cos(b_γ)*cos(α_1)*sin(θ_1)-sin(b_γ)*cos(α_1)*cos(θ_1))*sin(θ_2)+sin(α_2)*(cos(b_γ)*cos(θ_1)-sin(b_γ)*sin(θ_1))*cos(θ_2))-r_2*(cos(b_γ)*cos(θ_1)-sin(b_γ)*sin(θ_1))*
sin(θ_2)+r_2*(-cos(b_γ)*cos(α_1)*sin(θ_1)-sin(b_γ)*cos(α_1)*cos(θ_1))*cos(θ_2);
   M_22 :=
r_3*(-cos(α_2)*(cos(b_γ)*cos(α_1)*cos(θ_1)-sin(b_γ)*cos(α_1)*sin(θ_1))*sin(θ_2)-cos(α_2)*(cos(b_γ)*sin(θ_1)+sin(b_γ)*cos(θ_1))*cos(θ_2))*sin(θ_3)+r_3*
((cos(b_γ)*cos(α_1)*cos(θ_1)-sin(b_γ)*cos(α_1)*sin(θ_1))*cos(θ_2)-(cos(b_γ)*sin(θ_1)+sin(b_γ)*cos(θ_1))*sin(θ_2))*cos(θ_3)+d_3*
(sin(α_2)*(cos(b_γ)*cos(α_1)*cos(θ_1)-sin(b_γ)*cos(α_1)*sin(θ_1))*sin(θ_2)+sin(α_2)*(cos(b_γ)*sin(θ_1)+sin(b_γ)*cos(θ_1))*cos(θ_2))-r_2*(cos(b_γ)*sin(θ_1)+sin(b_γ)*cos(θ_1))*
sin(θ_2)+r_2*(cos(b_γ)*cos(α_1)*cos(θ_1)-sin(b_γ)*cos(α_1)*sin(θ_1))*cos(θ_2);
   M_32 :=
-r_3*sin(α_1)*cos(α_2)*sin(θ_2)*sin(θ_3)+r_3*sin(α_1)*cos(θ_2)*cos(θ_3)+d_3*sin(α_1)*sin(α_2)*sin(θ_2)+r_2*sin(α_1)*cos(θ_2);

   M_13 :=
r_3*(-cos(α_2)*(cos(b_γ)*cos(θ_1)-sin(b_γ)*sin(θ_1))*sin(θ_2)+cos(α_2)*(-cos(b_γ)*cos(α_1)*sin(θ_1)-sin(b_γ)*cos(α_1)*cos(θ_1))*cos(θ_2)+sin(α_2)*
(cos(b_γ)*sin(α_1)*sin(θ_1)+sin(b_γ)*sin(α_1)*cos(θ_1)))*cos(θ_3)-r_3*
((-cos(b_γ)*cos(α_1)*sin(θ_1)-sin(b_γ)*cos(α_1)*cos(θ_1))*sin(θ_2)+(cos(b_γ)*cos(θ_1)-sin(b_γ)*sin(θ_1))*cos(θ_2))*sin(θ_3);
   M_23 :=
r_3*(-cos(α_2)*(cos(b_γ)*sin(θ_1)+sin(b_γ)*cos(θ_1))*sin(θ_2)+cos(α_2)*(cos(b_γ)*cos(α_1)*cos(θ_1)-sin(b_γ)*cos(α_1)*sin(θ_1))*cos(θ_2)+sin(α_2)*
(sin(b_γ)*sin(α_1)*sin(θ_1)-cos(b_γ)*sin(α_1)*cos(θ_1)))*cos(θ_3)-r_3*
((cos(b_γ)*cos(α_1)*cos(θ_1)-sin(b_γ)*cos(α_1)*sin(θ_1))*sin(θ_2)+(cos(b_γ)*sin(θ_1)+sin(b_γ)*cos(θ_1))*cos(θ_2))*sin(θ_3);
   M_33 :=
r_3*(sin(α_1)*cos(α_2)*cos(θ_2)+cos(α_1)*sin(α_2))*cos(θ_3)-r_3*sin(α_1)*sin(θ_2)*sin(θ_3);
end Kinematics.Generic_Linear_Velocity_Jacobian;
