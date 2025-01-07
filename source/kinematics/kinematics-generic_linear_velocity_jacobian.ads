--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Computes coefficients of the linear velocity part of the Jacobian matrix of
--  the single leg.

pragma Restrictions (No_Elaboration_Code);

generic
   Cos_Base_Gamma : CGK.Reals.Real;
   Sin_Base_Gamma : CGK.Reals.Real;

   R_1            : CGK.Reals.Real;
   Cos_Alpha_1    : CGK.Reals.Real;
   Sin_Alpha_1    : CGK.Reals.Real;

   D_2            : CGK.Reals.Real;
   R_2            : CGK.Reals.Real;
   Cos_Alpha_2    : CGK.Reals.Real;
   Sin_Alpha_2    : CGK.Reals.Real;

   D_3            : CGK.Reals.Real;
   R_3            : CGK.Reals.Real;

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
  with Pure;
