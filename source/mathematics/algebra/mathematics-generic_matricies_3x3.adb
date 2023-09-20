--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Mathematics.Generic_Matricies_3x3 is

   -------------
   -- Product --
   -------------

   function Product (Left : Matrix_3x3; Right : Matrix_3x3) return Matrix_3x3 is
      L_11 : Real_Type renames Left.M_11;
      L_12 : Real_Type renames Left.M_12;
      L_13 : Real_Type renames Left.M_13;
      L_21 : Real_Type renames Left.M_21;
      L_22 : Real_Type renames Left.M_22;
      L_23 : Real_Type renames Left.M_23;
      L_31 : Real_Type renames Left.M_31;
      L_32 : Real_Type renames Left.M_32;
      L_33 : Real_Type renames Left.M_33;

      R_11 : Real_Type renames Right.M_11;
      R_12 : Real_Type renames Right.M_12;
      R_13 : Real_Type renames Right.M_13;
      R_21 : Real_Type renames Right.M_21;
      R_22 : Real_Type renames Right.M_22;
      R_23 : Real_Type renames Right.M_23;
      R_31 : Real_Type renames Right.M_31;
      R_32 : Real_Type renames Right.M_32;
      R_33 : Real_Type renames Right.M_33;

   begin
      return
        (M_11 => L_11 * R_11 + L_12 * R_21 + L_13 * R_31,
         M_12 => L_11 * R_12 + L_12 * R_22 + L_13 * R_32,
         M_13 => L_11 * R_13 + L_12 * R_23 + L_13 * R_33,

         M_21 => L_21 * R_11 + L_22 * R_21 + L_23 * R_31,
         M_22 => L_21 * R_12 + L_22 * R_22 + L_23 * R_32,
         M_23 => L_21 * R_13 + L_22 * R_23 + L_23 * R_33,

         M_31 => L_31 * R_11 + L_32 * R_21 + L_33 * R_31,
         M_32 => L_31 * R_12 + L_32 * R_22 + L_33 * R_32,
         M_33 => L_31 * R_13 + L_32 * R_23 + L_33 * R_33);
   end Product;

   -------------
   -- Product --
   -------------

   function Product
     (Left  : Matrix_3x3;
      Right : Vectors_3.Vector_3) return Vectors_3.Vector_3
   is
      L_11 : Real_Type renames Left.M_11;
      L_12 : Real_Type renames Left.M_12;
      L_13 : Real_Type renames Left.M_13;
      L_21 : Real_Type renames Left.M_21;
      L_22 : Real_Type renames Left.M_22;
      L_23 : Real_Type renames Left.M_23;
      L_31 : Real_Type renames Left.M_31;
      L_32 : Real_Type renames Left.M_32;
      L_33 : Real_Type renames Left.M_33;

      R_1  : Real_Type renames Right.M_1;
      R_2  : Real_Type renames Right.M_2;
      R_3  : Real_Type renames Right.M_3;

   begin
      return
        (L_11 * R_1 + L_12 * R_2 + L_13 * R_3,
         L_21 * R_1 + L_22 * R_2 + L_23 * R_3,
         L_31 * R_1 + L_32 * R_2 + L_33 * R_3);
   end Product;

end Mathematics.Generic_Matricies_3x3;
