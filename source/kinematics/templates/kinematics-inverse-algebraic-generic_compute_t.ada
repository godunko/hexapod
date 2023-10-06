--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

procedure Kinematics.Inverse.Algebraic.Generic_Compute_T
  (E_X   : Reals.Real;
   E_Y   : Reals.Real;
   E_Z   : Reals.Real;
   T_1   : out Reals.Real;
   T_2   : out Reals.Real;
   T_3   : out Reals.Real;
   T_4   : out Reals.Real;
   Count : out Natural)
is

   use type Reals.Real;

   function Sqrt (Item : Reals.Real) return Reals.Real
     renames Reals.Elementary_Functions.Sqrt;

   D_1 : constant Reals.Real :=
e_y^2-2*b_y*e_y+e_x^2-2*b_x*e_x+b_y^2+b_x^2;

   D_2 : constant Reals.Real :=
e_z^4+(2*e_y^2-4*b_y*e_y+2*e_x^2-4*b_x*e_x-2*r_3^2+4*r_2*r_3-2*r_2^2+2*r_1^2+2*b_y^2+2*b_x^2)*e_z^2+e_y^4-4*b_y*e_y^3+(2*e_x^2-4*b_x*e_x-2*r_3^2+4*r_2*r_3-2*r_2^2-2*r_1^2+6*b_y^2+2*b_x^2)*e_y^2+(-4*b_y*e_x^2+8*b_x*b_y*e_x+4*b_y*r_3^2-8*b_y*r_2*r_3+4*b_y*r_2^2+4*b_y*r_1^2-4*b_y^3-4*b_x^2*b_y)*e_y+e_x^4-4*b_x*e_x^3+(-2*r_3^2+4*r_2*r_3-2*r_2^2-2*r_1^2+2*b_y^2+6*b_x^2)*e_x^2+(4*b_x*r_3^2-8*b_x*r_2*r_3+4*b_x*r_2^2+4*b_x*r_1^2-4*b_x*b_y^2-4*b_x^3)*e_x+r_3^4-4*r_2*r_3^3+(6*r_2^2-2*r_1^2-2*b_y^2-2*b_x^2)*r_3^2+((4*r_1^2+4*b_y^2+4*b_x^2)*r_2-4*r_2^3)*r_3+r_2^4+(-2*r_1^2-2*b_y^2-2*b_x^2)*r_2^2+r_1^4+(-2*b_y^2-2*b_x^2)*r_1^2+b_y^4+2*b_x^2*b_y^2+b_x^4;

begin
   Count := 0;

   if D_1 < 0.0 then
      --  Square root computation will recult in complex value.

      raise Constraint_Error;
   end if;

   if abs D_2 < Reals.Real'Epsilon then
      --  Division by zero,

      raise Constraint_Error;
   end if;

   declare
      S_1 : constant Reals.Real :=
-e_z^4+(-2*e_y^2+4*b_y*e_y-2*e_x^2+4*b_x*e_x+2*r_3^2+2*r_2^2-2*r_1^2-2*b_y^2-2*b_x^2)*e_z^2-e_y^4+4*b_y*e_y^3+(-2*e_x^2+4*b_x*e_x+2*r_3^2+2*r_2^2+2*r_1^2-6*b_y^2-2*b_x^2)*e_y^2+(4*b_y*e_x^2-8*b_x*b_y*e_x-4*b_y*r_3^2-4*b_y*r_2^2-4*b_y*r_1^2+4*b_y^3+4*b_x^2*b_y)*e_y-e_x^4+4*b_x*e_x^3+(2*r_3^2+2*r_2^2+2*r_1^2-2*b_y^2-6*b_x^2)*e_x^2+(-4*b_x*r_3^2-4*b_x*r_2^2-4*b_x*r_1^2+4*b_x*b_y^2+4*b_x^3)*e_x-r_3^4+(2*r_2^2+2*r_1^2+2*b_y^2+2*b_x^2)*r_3^2-r_2^4+(2*r_1^2+2*b_y^2+2*b_x^2)*r_2^2-r_1^4+(2*b_y^2+2*b_x^2)*r_1^2-b_y^4-2*b_x^2*b_y^2-b_x^4;
      S_2 : constant Reals.Real :=
8*r_1*r_2*r_3*sqrt(D_1);

      V_12 : constant Reals.Real := (S_1 + S_2) / D_2;
      V_34 : constant Reals.Real := (S_1 - S_2) / D_2;

   begin
      --  Likewise, ignore complex solutions.

      if V_12 >= 0.0 then
         T_1   := -Sqrt (V_12);
         T_2   := Sqrt (V_12);
         Count := @ + 2;

         if V_34 >= 0.0 then
            T_3   := -Sqrt (V_34);
            T_4   := Sqrt (V_34);
            Count := @ + 2;
         end if;

      elsif V_34 >= 0.0 then
         T_3   := -Sqrt (V_34);
         T_4   := Sqrt (V_34);
         Count := @ + 2;
      end if;
   end;
end Kinematics.Inverse.Algebraic.Generic_Compute_T;
