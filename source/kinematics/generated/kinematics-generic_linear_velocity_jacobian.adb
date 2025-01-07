--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX - License - Identifier: Apache - 2.0
--

--  This file is create from formulas in `kinematics - jacobian.wxmx` file

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
R_3 * (-Cos_Alpha_2 * (-Cos_Base_Gamma * Sin_Theta_1 - Sin_Base_Gamma * Cos_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (Sin_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Cos_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Cos_Base_Gamma * Sin_Alpha_1 * Cos_Theta_1 - Sin_Base_Gamma * Sin_Alpha_1 * Sin_Theta_1)) * Sin_Theta_3 + R_3 *
((Sin_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Cos_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Sin_Theta_2 + (-Cos_Base_Gamma * Sin_Theta_1 - Sin_Base_Gamma * Cos_Theta_1) * Cos_Theta_2) * Cos_Theta_3 + d_3 * (Sin_Alpha_2 * (-Cos_Base_Gamma * Sin_Theta_1 - Sin_Base_Gamma * Cos_Theta_1) *
Sin_Theta_2 - Sin_Alpha_2 * (Sin_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Cos_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Cos_Theta_2 + Cos_Alpha_2 * (Cos_Base_Gamma * Sin_Alpha_1 * Cos_Theta_1 - Sin_Base_Gamma * Sin_Alpha_1 * Sin_Theta_1)) + R_2 *
(Sin_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Cos_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Sin_Theta_2 + R_2 * (-Cos_Base_Gamma * Sin_Theta_1 - Sin_Base_Gamma * Cos_Theta_1) * Cos_Theta_2 + d_2 * (Cos_Base_Gamma * Sin_Alpha_1 * Cos_Theta_1 - Sin_Base_Gamma * Sin_Alpha_1 * Sin_Theta_1)-
R_1 * Cos_Base_Gamma * Sin_Theta_1 - R_1 * Sin_Base_Gamma * Cos_Theta_1;
   M_21 :=
R_3 * (-Cos_Alpha_2 * (Cos_Base_Gamma * Cos_Theta_1 - Sin_Base_Gamma * Sin_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (-Cos_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Cos_Base_Gamma * Sin_Alpha_1 * Sin_Theta_1 + Sin_Base_Gamma * Sin_Alpha_1 * Cos_Theta_1)) * Sin_Theta_3 + R_3 *
((-Cos_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Sin_Theta_2 + (Cos_Base_Gamma * Cos_Theta_1 - Sin_Base_Gamma * Sin_Theta_1) * Cos_Theta_2) * Cos_Theta_3 + d_3 * (Sin_Alpha_2 * (Cos_Base_Gamma * Cos_Theta_1 - Sin_Base_Gamma * Sin_Theta_1) *
Sin_Theta_2 - Sin_Alpha_2 * (-Cos_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Cos_Theta_2 + Cos_Alpha_2 * (Cos_Base_Gamma * Sin_Alpha_1 * Sin_Theta_1 + Sin_Base_Gamma * Sin_Alpha_1 * Cos_Theta_1)) + R_2 *
(-Cos_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Sin_Theta_2 + R_2 * (Cos_Base_Gamma * Cos_Theta_1 - Sin_Base_Gamma * Sin_Theta_1) * Cos_Theta_2 + d_2 * (Cos_Base_Gamma * Sin_Alpha_1 * Sin_Theta_1 + Sin_Base_Gamma * Sin_Alpha_1 * Cos_Theta_1)-
R_1 * Sin_Base_Gamma * Sin_Theta_1 + R_1 * Cos_Base_Gamma * Cos_Theta_1;
   M_31 := 0.0;

   M_12 :=
R_3 * (-Cos_Alpha_2 * (-Cos_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Sin_Theta_2 - Cos_Alpha_2 * (Cos_Base_Gamma * Cos_Theta_1 - Sin_Base_Gamma * Sin_Theta_1) * Cos_Theta_2) * Sin_Theta_3 + R_3 *
((-Cos_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Cos_Theta_2 - (Cos_Base_Gamma * Cos_Theta_1 - Sin_Base_Gamma * Sin_Theta_1) * Sin_Theta_2) * Cos_Theta_3 + d_3 *
(Sin_Alpha_2 * (-Cos_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Sin_Theta_2 + Sin_Alpha_2 * (Cos_Base_Gamma * Cos_Theta_1 - Sin_Base_Gamma * Sin_Theta_1) * Cos_Theta_2) - R_2 * (Cos_Base_Gamma * Cos_Theta_1 - Sin_Base_Gamma * Sin_Theta_1) *
Sin_Theta_2 + R_2 * (-Cos_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Cos_Theta_2;
   M_22 :=
R_3 * (-Cos_Alpha_2 * (Cos_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1) * Sin_Theta_2 - Cos_Alpha_2 * (Cos_Base_Gamma * Sin_Theta_1 + Sin_Base_Gamma * Cos_Theta_1) * Cos_Theta_2) * Sin_Theta_3 + R_3 *
((Cos_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1) * Cos_Theta_2 - (Cos_Base_Gamma * Sin_Theta_1 + Sin_Base_Gamma * Cos_Theta_1) * Sin_Theta_2) * Cos_Theta_3 + d_3 *
(Sin_Alpha_2 * (Cos_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1) * Sin_Theta_2 + Sin_Alpha_2 * (Cos_Base_Gamma * Sin_Theta_1 + Sin_Base_Gamma * Cos_Theta_1) * Cos_Theta_2) - R_2 * (Cos_Base_Gamma * Sin_Theta_1 + Sin_Base_Gamma * Cos_Theta_1) *
Sin_Theta_2 + R_2 * (Cos_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1) * Cos_Theta_2;
   M_32 :=
-R_3 * Sin_Alpha_1 * Cos_Alpha_2 * Sin_Theta_2 * Sin_Theta_3 + R_3 * Sin_Alpha_1 * Cos_Theta_2 * Cos_Theta_3 + d_3 * Sin_Alpha_1 * Sin_Alpha_2 * Sin_Theta_2 + R_2 * Sin_Alpha_1 * Cos_Theta_2;

   M_13 :=
R_3 * (-Cos_Alpha_2 * (Cos_Base_Gamma * Cos_Theta_1 - Sin_Base_Gamma * Sin_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (-Cos_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Cos_Base_Gamma * Sin_Alpha_1 * Sin_Theta_1 + Sin_Base_Gamma * Sin_Alpha_1 * Cos_Theta_1)) * Cos_Theta_3 - R_3 *
((-Cos_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1) * Sin_Theta_2 + (Cos_Base_Gamma * Cos_Theta_1 - Sin_Base_Gamma * Sin_Theta_1) * Cos_Theta_2) * Sin_Theta_3;
   M_23 :=
R_3 * (-Cos_Alpha_2 * (Cos_Base_Gamma * Sin_Theta_1 + Sin_Base_Gamma * Cos_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (Cos_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Sin_Base_Gamma * Sin_Alpha_1 * Sin_Theta_1 - Cos_Base_Gamma * Sin_Alpha_1 * Cos_Theta_1)) * Cos_Theta_3 - R_3 *
((Cos_Base_Gamma * Cos_Alpha_1 * Cos_Theta_1 - Sin_Base_Gamma * Cos_Alpha_1 * Sin_Theta_1) * Sin_Theta_2 + (Cos_Base_Gamma * Sin_Theta_1 + Sin_Base_Gamma * Cos_Theta_1) * Cos_Theta_2) * Sin_Theta_3;
   M_33 :=
R_3 * (Sin_Alpha_1 * Cos_Alpha_2 * Cos_Theta_2 + Cos_Alpha_1 * Sin_Alpha_2) * Cos_Theta_3 - R_3 * Sin_Alpha_1 * Sin_Theta_2 * Sin_Theta_3;
end Kinematics.Generic_Linear_Velocity_Jacobian;
