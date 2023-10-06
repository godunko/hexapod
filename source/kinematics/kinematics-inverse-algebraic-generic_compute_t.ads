--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Kinematics.Configuration;

generic
   B_X : Reals.Real;
   B_Y : Reals.Real;
   R_1 : Reals.Real;
   R_2 : Reals.Real;
   R_3 : Reals.Real;

procedure Kinematics.Inverse.Algebraic.Generic_Compute_T
  (E_X   : Reals.Real;
   E_Y   : Reals.Real;
   E_Z   : Reals.Real;
   T_1   : out Reals.Real;
   T_2   : out Reals.Real;
   T_3   : out Reals.Real;
   T_4   : out Reals.Real;
   Count : out Natural);
