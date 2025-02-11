--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with VSS.Strings;
with VSS.String_Vectors;

package body GUI.Programs.Lines is

   Text_V : constant VSS.String_Vectors.Virtual_String_Vector :=
     ["#version 130",
      "uniform mat4 mvp;",
      "in vec3 vp;",
      "void main() {",
      "  gl_Position = mvp * vec4(vp, 1.0);",
      "}"];
   Text_F : constant VSS.String_Vectors.Virtual_String_Vector :=
     ["#version 130",
      "uniform vec4 c;",
      "out vec4 frag_colour;",
      "void main() {",
      "  frag_colour = c;",
      "}"];

   MVP_Name : constant VSS.Strings.Virtual_String := "mvp";
   C_Name   : constant VSS.Strings.Virtual_String := "c";
   VP_Name  : constant VSS.Strings.Virtual_String := "vp";

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Line_Program'Class) is
   begin
      Self.Add_Shader_From_Source_Code (OpenGL.Vertex, Text_V);
      Self.Add_Shader_From_Source_Code (OpenGL.Fragment, Text_F);
   end Initialize;

   ----------
   -- Link --
   ----------

   overriding function Link (Self : in out Line_Program) return Boolean is
   begin
      if not OpenGL.Programs.OpenGL_Program (Self).Link then
         return False;
      end if;

      Self.MVP := Self.Uniform_Location (MVP_Name);
      Self.C   := Self.Uniform_Location (C_Name);
      Self.VP  := Self.Attribute_Location (VP_Name);

      return True;
   end Link;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Self : in out Line_Program'Class;
      To   : OpenGL.GLubyte_Vector_4)
   is
      use type OpenGL.GLfloat;

      C : constant OpenGL.GLfloat_Vector_4 :=
        [for J in To'Range => OpenGL.GLfloat (To (J)) / 255.0];

   begin
      Self.Set_Uniform_Value (Self.C, C);
   end Set_Color;

   -------------
   -- Set_MVP --
   -------------

   procedure Set_MVP
     (Self : in out Line_Program'Class;
      MVP  : OpenGL.GLfloat_Matrix_4x4) is
   begin
      Self.Set_Uniform_Value (Self.MVP, MVP);
   end Set_MVP;

   ----------------------------
   -- Set_Vertex_Data_Buffer --
   ----------------------------

   procedure Set_Vertex_Data_Buffer
     (Self   : in out Line_Program'Class;
      Buffer : in out Vertex_Data_Buffers.OpenGL_Buffer'Class)
   is
      Dummy : Vertex_Data;

   begin
      Buffer.Bind;

      Self.Enable_Attribute_Array (Self.VP);

      Self.Set_Attribute_Buffer
       (Location   => Self.VP,
        Data_Type  => OpenGL.GL_FLOAT,
        Tuple_Size => Dummy.VP'Length,
        Offset     => Dummy.VP'Position,
        Stride     => Vertex_Data_Buffers.Stride);
   end Set_Vertex_Data_Buffer;

end GUI.Programs.Lines;
