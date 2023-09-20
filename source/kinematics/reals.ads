--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics.Generic_Elementary_Functions;
with Interfaces;

with Mathematics.Generic_Matricies_3x3;
with Mathematics.Generic_Transformations_3D.Generic_HD_Constructors;
with Mathematics.Generic_Vectors_3;
with Mathematics.Generic_Vectors_3D;

package Reals is

   pragma Pure;

   type Real is new Interfaces.IEEE_Float_32;

   package Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Real);

   package Vectors_3 is
     new Mathematics.Generic_Vectors_3 (Real);

   package Matricies_3x3 is
     new Mathematics.Generic_Matricies_3x3 (Real, Vectors_3);

   package Vectors_3D is
     new Mathematics.Generic_Vectors_3D (Real, Vectors_3);

   package Transformations_3D is
     new Mathematics.Generic_Transformations_3D
           (Real, Vectors_3, Matricies_3x3, Vectors_3D);

   package Transformation_3D_HD_Constructors is
     new Transformations_3D.Generic_HD_Constructors (Elementary_Functions);

end Reals;
