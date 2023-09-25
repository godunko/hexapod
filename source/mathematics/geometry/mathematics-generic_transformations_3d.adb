--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Mathematics.Generic_Transformations_3D is

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Transformation_3D;
      Right : Transformation_3D) return Transformation_3D is
   begin
      return
        (Q => Matricies_3x3.Product (Left.Q, Right.Q),
         A =>
           --  XXX Can it be rewritten with Vector_4?
           (M_1 =>
              Left.Q.M_11 * Right.A.M_1
                + Left.Q.M_12 * Right.A.M_2
                + Left.Q.M_13 * Right.A.M_3
                + Left.A.M_1,
            M_2 =>
              Left.Q.M_21 * Right.A.M_1
                + Left.Q.M_22 * Right.A.M_2
                + Left.Q.M_23 * Right.A.M_3
                + Left.A.M_2,
            M_3 =>
              Left.Q.M_31 * Right.A.M_1
                + Left.Q.M_32 * Right.A.M_2
                + Left.Q.M_33 * Right.A.M_3
                + Left.A.M_3));
   end "*";

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : out Transformation_3D;
      M_11 : Real_Type;
      M_12 : Real_Type;
      M_13 : Real_Type;
      M_14 : Real_Type;
      M_21 : Real_Type;
      M_22 : Real_Type;
      M_23 : Real_Type;
      M_24 : Real_Type;
      M_31 : Real_Type;
      M_32 : Real_Type;
      M_33 : Real_Type;
      M_34 : Real_Type) is
   begin
      Self.Q :=
        (M_11 => M_11,
         M_12 => M_12,
         M_13 => M_13,
         M_21 => M_21,
         M_22 => M_22,
         M_23 => M_23,
         M_31 => M_31,
         M_32 => M_32,
         M_33 => M_33);
      Self.A :=
        (M_1 => M_14,
         M_2 => M_24,
         M_3 => M_34);
   end Initialize;

   ---------------
   -- Transform --
   ---------------

   function Transform
     (Self : Transformation_3D;
      Item : Vectors_3D.Vector_3D) return Vectors_3D.Vector_3D is
   begin
      return
        Vectors_3D.To_Vector_3D
          (Vectors_3.Sum
             (Matricies_3x3.Product (Self.Q, Vectors_3D.As_Vector_3 (Item)),
              Self.A));
   end Transform;

end Mathematics.Generic_Transformations_3D;
