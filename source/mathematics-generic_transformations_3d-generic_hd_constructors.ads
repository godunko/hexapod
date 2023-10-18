--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics.Generic_Elementary_Functions;

with Mathematics.Generic_Matricies_3x3;
with Mathematics.Generic_Vectors_3;
with Mathematics.Generic_Vectors_3D;

generic
   with package Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Real_Type);

package Mathematics.Generic_Transformations_3D.Generic_HD_Constructors is

   pragma Pure;

   function Create_Transformation_3D
     (d : Real_Type;
      θ : Real_Type;
      r : Real_Type;
      α : Real_Type) return Transformation_3D;
   --  Creates Denavit-Hartenberg transformation.

end Mathematics.Generic_Transformations_3D.Generic_HD_Constructors;
