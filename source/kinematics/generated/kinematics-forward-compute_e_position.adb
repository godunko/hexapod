--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX - License - Identifier: Apache - 2.0
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
R_3 * (-Cos_Alpha_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (-Cos_Alpha_0 * Cos_Alpha_1 * Sin_Theta_1 - Sin_Alpha_0 * Cos_Alpha_1 * Cos_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Cos_Alpha_0 * Sin_Alpha_1 * Sin_Theta_1 + Sin_Alpha_0 * Sin_Alpha_1 * Cos_Theta_1)) * Sin_Theta_3 + R_3 * ((-Cos_Alpha_0 * Cos_Alpha_1 * Sin_Theta_1 - Sin_Alpha_0 * Cos_Alpha_1 * Cos_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2) * Cos_Theta_3 + 
R_2 * (-Cos_Alpha_0 * Cos_Alpha_1 * Sin_Theta_1 - Sin_Alpha_0 * Cos_Alpha_1 * Cos_Theta_1) * Sin_Theta_2 + R_2 * (Cos_Alpha_0 * Cos_Theta_1 - Sin_Alpha_0 * Sin_Theta_1) * Cos_Theta_2 - R_1 * Sin_Alpha_0 * Sin_Theta_1 + R_1 * Cos_Alpha_0 * Cos_Theta_1 + B_x;

   Y :=
R_3 * (-Cos_Alpha_2 * (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Sin_Theta_2 + Cos_Alpha_2 * (Cos_Alpha_0 * Cos_Alpha_1 * Cos_Theta_1 - Sin_Alpha_0 * Cos_Alpha_1 * Sin_Theta_1) * Cos_Theta_2 + Sin_Alpha_2 *
(Sin_Alpha_0 * Sin_Alpha_1 * Sin_Theta_1 - Cos_Alpha_0 * Sin_Alpha_1 * Cos_Theta_1)) * Sin_Theta_3 + R_3 * ((Cos_Alpha_0 * Cos_Alpha_1 * Cos_Theta_1 - Sin_Alpha_0 * Cos_Alpha_1 * Sin_Theta_1) * Sin_Theta_2 + (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2) * Cos_Theta_3 + 
R_2 * (Cos_Alpha_0 * Cos_Alpha_1 * Cos_Theta_1 - Sin_Alpha_0 * Cos_Alpha_1 * Sin_Theta_1) * Sin_Theta_2 + R_2 * (Cos_Alpha_0 * Sin_Theta_1 + Sin_Alpha_0 * Cos_Theta_1) * Cos_Theta_2 + R_1 * Cos_Alpha_0 * Sin_Theta_1 + R_1 * Sin_Alpha_0 * Cos_Theta_1 + B_y;

   Z :=
R_3 * (Sin_Alpha_1 * Cos_Alpha_2 * Cos_Theta_2 + Cos_Alpha_1 * Sin_Alpha_2) * Sin_Theta_3 + R_3 * Sin_Alpha_1 * Sin_Theta_2 * Cos_Theta_3 + R_2 * Sin_Alpha_1 * Sin_Theta_2 + B_z;
end Compute_E_Position;
