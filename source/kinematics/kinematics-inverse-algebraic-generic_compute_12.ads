--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Kinematics.Configuration;

generic
   B_X         : Reals.Real;
   B_Y         : Reals.Real;
   B_Z         : Reals.Real;
   Cos_Gamma_0 : Reals.Real;
   Sin_Gamma_0 : Reals.Real;
   Cos_Alpha_1 : Reals.Real;
   Sin_Alpha_1 : Reals.Real;
   Cos_Alpha_2 : Reals.Real;
   Sin_Alpha_2 : Reals.Real;
   D_1         : Reals.Real;
   D_2         : Reals.Real;
   D_3         : Reals.Real;
   R_1         : Reals.Real;
   R_2         : Reals.Real;
   R_3         : Reals.Real;

procedure Kinematics.Inverse.Algebraic.Generic_Compute_12
  (E_X         : Reals.Real;
   E_Y         : Reals.Real;
   E_Z         : Reals.Real;
   Cos_Theta_3 : Reals.Real;
   Sin_Theta_3 : Reals.Real;
   Theta_1     : out Reals.Real;
   Theta_2     : out Reals.Real);
