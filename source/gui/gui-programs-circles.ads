--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  OpenGL program to draw circle using geometry shader

with OpenGL.Generic_Buffers;
with OpenGL.Programs;

package GUI.Programs.Circles is

   type Vertex_Data is record
      Center : OpenGL.GLfloat_Vector_2;  --  Center Position
      Radius : OpenGL.GLfloat;           --  Radius of the circle
   end record with Pack;

   type Vertex_Data_Array is array (Positive range <>) of Vertex_Data;

   package Vertex_Data_Buffers is
     new OpenGL.Generic_Buffers (Vertex_Data, Positive, Vertex_Data_Array);

   type Circle_Program is new OpenGL.Programs.OpenGL_Program with private;

   procedure Initialize (Self : in out Circle_Program'Class);
   --  Initialize program object.

   procedure Set_Vertex_Data_Buffer
    (Self   : in out Circle_Program'Class;
     Buffer : in out Vertex_Data_Buffers.OpenGL_Buffer'Class);
   --  Sets buffer with data to draw.

   procedure Set_MVP
     (Self : in out Circle_Program'Class;
      MVP  : OpenGL.GLfloat_Matrix_4x4);
   --  Sets Model-View-Projection matrix

   procedure Set_Color
     (Self : in out Circle_Program'Class;
      To   : OpenGL.GLubyte_Vector_3);
   --  Sets color to be used to draw lines

private

   type Circle_Program is new OpenGL.Programs.OpenGL_Program with record
      MVP    : OpenGL.Uniform_Location;
      Color  : OpenGL.Uniform_Location;
      Center : OpenGL.Attribute_Location;
      Radius : OpenGL.Attribute_Location;
   end record;

   overriding function Link (Self : in out Circle_Program) return Boolean;
   --  Executed at link time. Link shaders into program and extracts locations
   --  of attributes.

end GUI.Programs.Circles;
