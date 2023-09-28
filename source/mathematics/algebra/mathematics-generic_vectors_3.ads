--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

generic
   type Real_Type is digits <>;

package Mathematics.Generic_Vectors_3 is

   pragma Pure;

   type Vector_3 is record
      M_1 : Real_Type;
      M_2 : Real_Type;
      M_3 : Real_Type;
   end record;

   function Add
     (Left : Vector_3; Right : Vector_3) return Vector_3 with Inline;

   function Subtract
     (Left : Vector_3; Right : Vector_3) return Vector_3 with Inline;

   function "-"
     (Left : Vector_3; Right : Vector_3) return Vector_3 with Inline;

private

   function Add
     (Left : Vector_3; Right : Vector_3) return Vector_3
       is (Left.M_1 + Right.M_1, Left.M_2 + Right.M_2, Left.M_3 + Right.M_3);

   function Subtract
     (Left : Vector_3; Right : Vector_3) return Vector_3
       is (Left.M_1 - Right.M_1, Left.M_2 - Right.M_2, Left.M_3 - Right.M_3);

   function "-"
     (Left : Vector_3; Right : Vector_3) return Vector_3 renames Subtract;

end Mathematics.Generic_Vectors_3;
