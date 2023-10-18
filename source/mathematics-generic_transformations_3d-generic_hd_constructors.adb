--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Mathematics.Generic_Transformations_3D.Generic_HD_Constructors is

   ------------------------------
   -- Create_Transformation_3D --
   ------------------------------

   function Create_Transformation_3D
     (d : Real_Type;
      θ : Real_Type;
      r : Real_Type;
      α : Real_Type) return Transformation_3D
   is
      Sin_θ : constant Real_Type := Elementary_Functions.Sin (θ);
      Cos_θ : constant Real_Type := Elementary_Functions.Cos (θ);
      Sin_α : constant Real_Type := Elementary_Functions.Sin (α);
      Cos_α : constant Real_Type := Elementary_Functions.Cos (α);

   begin
      return
        (Q =>
           (M_11 => Cos_θ,     M_12 => -Sin_θ * Cos_α, M_13 => Sin_θ * Sin_α,
            M_21 => Sin_θ,     M_22 => Cos_θ * Cos_α,  M_23 => -Cos_θ * Sin_α,
            M_31 => 0.0,       M_32 => Sin_α,          M_33 => Cos_α),
         A =>
           (M_1  => r * Cos_θ, M_2  => r * Sin_θ,      M_3  => d));
   end Create_Transformation_3D;

end Mathematics.Generic_Transformations_3D.Generic_HD_Constructors;
