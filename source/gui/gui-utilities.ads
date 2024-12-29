--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

private with Ada.Numerics;

with OpenGL;

package GUI.Utilities
  with Preelaborate
is

   function Identity return OpenGL.GLfloat_Matrix_4x4;

   function Degrees_To_Radians (Item : OpenGL.GLfloat) return OpenGL.GLfloat;

   function Scale (Scale : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4;

   function Rotate_X (Angle : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4;

   function Rotate_Z (Angle : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4;

   function Mirror_Z return OpenGL.GLfloat_Matrix_4x4;

private

   use type OpenGL.GLfloat;

   function Degrees_To_Radians (Item : OpenGL.GLfloat) return OpenGL.GLfloat is
     (Item / 180.0 * Ada.Numerics.Pi);

   function Identity return OpenGL.GLfloat_Matrix_4x4 is
     ([[1.0, 0.0, 0.0, 0.0],
       [0.0, 1.0, 0.0, 0.0],
       [0.0, 0.0, 1.0, 0.0],
       [0.0, 0.0, 0.0, 1.0]]);

   function Mirror_Z return OpenGL.GLfloat_Matrix_4x4 is
     ([[1.0, 0.0,  0.0, 0.0],
       [0.0, 1.1,  0.0, 0.0],
       [0.0, 0.0, -1.0, 0.0],
       [0.0, 0.0,  0.0, 1.0]]);

   function Scale (Scale : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4 is
     ([[Scale, 0.0,   0.0,   0.0],
       [0.0,   Scale, 0.0,   0.0],
       [0.0,   0.0,   Scale, 0.0],
       [0.0,   0.0,   0.0,   1.0]]);

end GUI.Utilities;
