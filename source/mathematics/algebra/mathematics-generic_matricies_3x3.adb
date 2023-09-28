--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Mathematics.Generic_Matricies_3x3 is

   procedure Determinant
     (Self : Matrix_3x3;
      M_11 : out Real_Type;
      N_12 : out Real_Type;
      M_13 : out Real_Type;
      D    : out Real_Type);
   --  Computes minors of the first row and determinant of the matrix.
   --  Note, N_12 is hegated value of the M_12.

   ---------
   -- Add --
   ---------

   function Add (Left : Matrix_3x3; Right : Matrix_3x3) return Matrix_3x3 is
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
        (M_11 => L_11 + R_11, M_12 => L_12 + R_12, M_13 => L_13 + R_13,
         M_21 => L_21 + R_21, M_22 => L_22 + R_22, M_23 => L_23 + R_23,
         M_31 => L_31 + R_31, M_32 => L_32 + R_32, M_33 => L_33 + R_33);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Left   : Matrix_3x3;
      Right  : Matrix_3x3;
      Result : out Matrix_3x3)
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
      Result.M_11 := L_11 + R_11;
      Result.M_12 := L_12 + R_12;
      Result.M_13 := L_13 + R_13;

      Result.M_21 := L_21 + R_21;
      Result.M_22 := L_22 + R_22;
      Result.M_23 := L_23 + R_23;

      Result.M_31 := L_31 + R_31;
      Result.M_32 := L_32 + R_32;
      Result.M_33 := L_33 + R_33;
   end Add;

   -----------------
   -- Determinant --
   -----------------

   procedure Determinant
     (Self : Matrix_3x3;
      M_11 : out Real_Type;
      N_12 : out Real_Type;
      M_13 : out Real_Type;
      D    : out Real_Type)
   is
      A_11 : Real_Type renames Self.M_11;
      A_12 : Real_Type renames Self.M_12;
      A_13 : Real_Type renames Self.M_13;
      A_21 : Real_Type renames Self.M_21;
      A_22 : Real_Type renames Self.M_22;
      A_23 : Real_Type renames Self.M_23;
      A_31 : Real_Type renames Self.M_31;
      A_32 : Real_Type renames Self.M_32;
      A_33 : Real_Type renames Self.M_33;

   begin
      M_11 := A_22 * A_33 - A_23 * A_32;
      N_12 := A_23 * A_31 - A_21 * A_33;
      M_13 := A_21 * A_32 - A_22 * A_31;
      D    := A_11 * M_11 + A_12 * N_12 + A_13 * M_13;
   end Determinant;

   -----------------
   -- Determinant --
   -----------------

   function Determinant (Self : Matrix_3x3) return Real_Type is
      M_11 : Real_Type;
      N_12 : Real_Type;
      M_13 : Real_Type;
      D    : Real_Type;

   begin
      Determinant
        (Self => Self, M_11 => M_11, N_12 => N_12, M_13 => M_13, D => D);

      return D;
   end Determinant;

   -------------
   -- Inverse --
   -------------

   function Inverse (Self : Matrix_3x3) return Matrix_3x3 is
      A_11 : Real_Type renames Self.M_11;
      A_12 : Real_Type renames Self.M_12;
      A_13 : Real_Type renames Self.M_13;
      A_21 : Real_Type renames Self.M_21;
      A_22 : Real_Type renames Self.M_22;
      A_23 : Real_Type renames Self.M_23;
      A_31 : Real_Type renames Self.M_31;
      A_32 : Real_Type renames Self.M_32;
      A_33 : Real_Type renames Self.M_33;

      M_11 : Real_Type;
      N_12 : Real_Type;
      M_13 : Real_Type;
      N_21 : Real_Type;
      M_22 : Real_Type;
      N_23 : Real_Type;
      M_31 : Real_Type;
      N_32 : Real_Type;
      M_33 : Real_Type;
      --  Minors (M_DD) and negated minors (N_DD) of the matrix.

      D    : Real_Type;
      --  Determinatat of the matrix.

   begin
      Determinant
        (Self => Self, M_11 => M_11, N_12 => N_12, M_13 => M_13, D => D);

      if D = 0.0 then
         raise Constraint_Error;
      end if;

      N_21 := A_13 * A_32 - A_12 * A_33;
      M_22 := A_11 * A_33 - A_13 * A_31;
      N_23 := A_12 * A_31 - A_11 * A_32;

      M_31 := A_12 * A_23 - A_13 * A_22;
      N_32 := A_13 * A_21 - A_11 * A_23;
      M_33 := A_11 * A_22 - A_12 * A_21;

      return
        (M_11 => M_11 / D, M_12 => N_21 / D, M_13 => M_31 / D,
         M_21 => N_12 / D, M_22 => M_22 / D, M_23 => N_32 / D,
         M_31 => M_13 / D, M_32 => N_23 / D, M_33 => M_33 / D);
   end Inverse;

   -------------
   -- Inverse --
   -------------

   procedure Inverse (Self : in out Matrix_3x3) is
      A_11 : Real_Type renames Self.M_11;
      A_12 : Real_Type renames Self.M_12;
      A_13 : Real_Type renames Self.M_13;
      A_21 : Real_Type renames Self.M_21;
      A_22 : Real_Type renames Self.M_22;
      A_23 : Real_Type renames Self.M_23;
      A_31 : Real_Type renames Self.M_31;
      A_32 : Real_Type renames Self.M_32;
      A_33 : Real_Type renames Self.M_33;

      M_11 : Real_Type;
      N_12 : Real_Type;
      M_13 : Real_Type;
      N_21 : Real_Type;
      M_22 : Real_Type;
      N_23 : Real_Type;
      M_31 : Real_Type;
      N_32 : Real_Type;
      M_33 : Real_Type;
      --  Minors (M_DD) and negated minors (N_DD) of the matrix.

      D    : Real_Type;
      --  Determinatat of the matrix.

   begin
      Determinant
        (Self => Self, M_11 => M_11, N_12 => N_12, M_13 => M_13, D => D);

      if D = 0.0 then
         raise Constraint_Error;
      end if;

      N_21 := A_13 * A_32 - A_12 * A_33;
      M_22 := A_11 * A_33 - A_13 * A_31;
      N_23 := A_12 * A_31 - A_11 * A_32;

      M_31 := A_12 * A_23 - A_13 * A_22;
      N_32 := A_13 * A_21 - A_11 * A_23;
      M_33 := A_11 * A_22 - A_12 * A_21;

      Self.M_11 := M_11 / D;
      Self.M_12 := N_21 / D;
      Self.M_13 := M_31 / D;
      Self.M_21 := N_12 / D;
      Self.M_22 := M_22 / D;
      Self.M_23 := N_32 / D;
      Self.M_31 := M_13 / D;
      Self.M_32 := N_23 / D;
      Self.M_33 := M_33 / D;
   end Inverse;

   -------------
   -- Product --
   -------------

   function Product (Left : Matrix_3x3; Right : Matrix_3x3) return Matrix_3x3 is
   begin
      return Result : Matrix_3x3 do
         Product (Left, Right, Result);
      end return;
   end Product;

   -------------
   -- Product --
   -------------

   procedure Product
     (Left   : Matrix_3x3;
      Right  : Matrix_3x3;
      Result : out Matrix_3x3)
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
      Result.M_11 := L_11 * R_11 + L_12 * R_21 + L_13 * R_31;
      Result.M_12 := L_11 * R_12 + L_12 * R_22 + L_13 * R_32;
      Result.M_13 := L_11 * R_13 + L_12 * R_23 + L_13 * R_33;

      Result.M_21 := L_21 * R_11 + L_22 * R_21 + L_23 * R_31;
      Result.M_22 := L_21 * R_12 + L_22 * R_22 + L_23 * R_32;
      Result.M_23 := L_21 * R_13 + L_22 * R_23 + L_23 * R_33;

      Result.M_31 := L_31 * R_11 + L_32 * R_21 + L_33 * R_31;
      Result.M_32 := L_31 * R_12 + L_32 * R_22 + L_33 * R_32;
      Result.M_33 := L_31 * R_13 + L_32 * R_23 + L_33 * R_33;
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

   ------------
   -- Scalar --
   ------------

   function Scalar (Value : Real_Type) return Matrix_3x3 is
   begin
      return
        (M_11   => Value,
         M_22   => Value,
         M_33   => Value,
         others => 0.0);
   end Scalar;

   ---------------
   -- Transpose --
   ---------------

   function Transpose (Self : Matrix_3x3) return Matrix_3x3 is
   begin
      return Result : Matrix_3x3 do
         Transpose (Self, Result);
      end return;
   end Transpose;

   ---------------
   -- Transpose --
   ---------------

   procedure Transpose
     (Self   : Matrix_3x3;
      Result : out Matrix_3x3)
   is
      M_11 : Real_Type renames Self.M_11;
      M_12 : Real_Type renames Self.M_12;
      M_13 : Real_Type renames Self.M_13;
      M_21 : Real_Type renames Self.M_21;
      M_22 : Real_Type renames Self.M_22;
      M_23 : Real_Type renames Self.M_23;
      M_31 : Real_Type renames Self.M_31;
      M_32 : Real_Type renames Self.M_32;
      M_33 : Real_Type renames Self.M_33;

   begin
      Result.M_11 := M_11;
      Result.M_12 := M_21;
      Result.M_13 := M_31;
      Result.M_21 := M_12;
      Result.M_22 := M_22;
      Result.M_23 := M_32;
      Result.M_31 := M_13;
      Result.M_32 := M_23;
      Result.M_33 := M_33;
   end Transpose;

end Mathematics.Generic_Matricies_3x3;
