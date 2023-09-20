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
