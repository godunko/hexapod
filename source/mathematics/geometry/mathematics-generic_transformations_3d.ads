--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Mathematics.Generic_Matricies_3x3;
with Mathematics.Generic_Vectors_3;
with Mathematics.Generic_Vectors_3D;

generic
   type Real_Type is digits <>;

   with package Vectors_3 is new Mathematics.Generic_Vectors_3 (Real_Type);

   with package Matricies_3x3 is
     new Mathematics.Generic_Matricies_3x3 (Real_Type, Vectors_3);

   with package Vectors_3D is
     new Mathematics.Generic_Vectors_3D (Real_Type, Vectors_3);

package Mathematics.Generic_Transformations_3D is

   pragma Pure;

   type Transformation_3D is private;

   function "*"
     (Left  : Transformation_3D;
      Right : Transformation_3D) return Transformation_3D;

   function Transform
     (Self : Transformation_3D;
      Item : Vectors_3D.Vector_3D) return Vectors_3D.Vector_3D;

private

   type Transformation_3D is record
      Q : Matricies_3x3.Matrix_3x3;
      A : Vectors_3.Vector_3;
   end record;

end Mathematics.Generic_Transformations_3D;
