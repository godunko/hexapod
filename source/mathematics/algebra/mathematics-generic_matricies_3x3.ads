--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Mathematics.Generic_Vectors_3;

generic
   type Real_Type is digits <>;

   with package Vectors_3 is new Mathematics.Generic_Vectors_3 (Real_Type);

package Mathematics.Generic_Matricies_3x3 is

   pragma Pure;

   type Matrix_3x3 is record
      M_11 : Real_Type;
      M_12 : Real_Type;
      M_13 : Real_Type;

      M_21 : Real_Type;
      M_22 : Real_Type;
      M_23 : Real_Type;

      M_31 : Real_Type;
      M_32 : Real_Type;
      M_33 : Real_Type;
   end record;

   function Product (Left : Matrix_3x3; Right : Matrix_3x3) return Matrix_3x3;

   function Product
     (Left  : Matrix_3x3;
      Right : Vectors_3.Vector_3) return Vectors_3.Vector_3;

end Mathematics.Generic_Matricies_3x3;
