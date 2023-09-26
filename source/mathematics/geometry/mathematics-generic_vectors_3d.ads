--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Mathematics.Generic_Vectors_3;

generic
   type Real_Type is digits <>;

   with package Vectors_3 is new Mathematics.Generic_Vectors_3 (Real_Type);

package Mathematics.Generic_Vectors_3D is

   pragma Pure;

   type Vector_3D is private;

   function To_Vector_3D
     (X : Real_Type;
      Y : Real_Type;
      Z : Real_Type) return Vector_3D with Inline;

   function To_Vector_3D
     (Item : Vectors_3.Vector_3) return Vector_3D with Inline;

   function As_Vector_3
     (Self : Vector_3D) return Vectors_3.Vector_3 with Inline;

   function X (Self : Vector_3D) return Real_Type with Inline;

   function Y (Self : Vector_3D) return Real_Type with Inline;

   function Z (Self : Vector_3D) return Real_Type with Inline;

   function "+"
     (Left : Vector_3D; Right : Vector_3D) return Vector_3D with Inline;

   function "-"
     (Left : Vector_3D; Right : Vector_3D) return Vector_3D with Inline;

private

   type Vector_3D is new Vectors_3.Vector_3;

   function "+"
     (Left : Vector_3D; Right : Vector_3D) return Vector_3D
        is (Add (Left, Right));

   function "-"
     (Left : Vector_3D; Right : Vector_3D) return Vector_3D
        is (Subtract (Left, Right));

   function As_Vector_3
     (Self : Vector_3D) return Vectors_3.Vector_3
        is (Vectors_3.Vector_3 (Self));

   function To_Vector_3D
     (X : Real_Type;
      Y : Real_Type;
      Z : Real_Type) return Vector_3D is (M_1 => X, M_2 => Y, M_3 => Z);

   function To_Vector_3D
     (Item : Vectors_3.Vector_3) return Vector_3D is (Vector_3D (Item));

   function X (Self : Vector_3D) return Real_Type is (Self.M_1);

   function Y (Self : Vector_3D) return Real_Type is (Self.M_2);

   function Z (Self : Vector_3D) return Real_Type is (Self.M_3);

end Mathematics.Generic_Vectors_3D;
