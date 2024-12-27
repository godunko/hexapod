--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with VSS.Strings;
with VSS.String_Vectors;

package body GUI.Programs.Circles is

   Vertex_Code   : constant VSS.String_Vectors.Virtual_String_Vector :=
     ["#version 130",
      "in vec2 center;",
      "in float radius;",
      "out float circle_radius;",
      "void main() {",
      "  gl_Position = vec4(center.x, center.y, 0.0, 1.0);",
      "  circle_radius = radius;",
      "}"];

   Geometry_Code : constant VSS.String_Vectors.Virtual_String_Vector :=
     ["#version 330",
      "in float circle_radius[];",
      "layout (points) in;",
      "layout (triangle_strip, max_vertices = 82) out;",
      "uniform mat4 mvp;",
      "void main() {",
      "  vec4 center = gl_in[0].gl_Position;",
      "  vec4 offset = vec4(center.x, center.y, -0.001, 0.0);",
      "  int segments = 40;",
      "  float PI = 3.14159265358;",
      "  float angle = PI / segments;",
      "  float c = cos (angle);",
      "  float s = sin (angle);",
      "  vec2 current = vec2 (circle_radius[0], 0.0);",
      "  mat2 step_rotation = mat2(vec2(c, s), vec2(-s, c));",
      "  vec4 point;",

      "  point = vec4(current.x, current.y, 0.0, 1.0);",
      "  gl_Position = mvp * (point + offset);",
      "  EmitVertex();",

      "  for (int j = 0; j < segments - 1; j++) {",
      "    current = step_rotation * current;",
      "    point = vec4(current.x, current.y, 0.0, 1.0);",
      "    gl_Position = mvp * (point + offset);",
      "    EmitVertex();",
      "    point = vec4(current.x, -current.y, 0.0, 1.0);",
      "    gl_Position = mvp * (point + offset);",
      "    EmitVertex();",
      "  }",

      "  point = vec4(-circle_radius[0], 0.0, 0.0, 1.0);",
      "  gl_Position = mvp * (point + offset);",
      "  EmitVertex();",
      "  EndPrimitive();",
      "}"];

   Fragment_Code : constant VSS.String_Vectors.Virtual_String_Vector :=
     ["#version 130",
      "uniform vec3 color;",
      "out vec4 frag_colour;",
      "void main() {",
      "  frag_colour = vec4(color, 1.0);",
      --  "  gl_FragDepth = -0.1;",
      "}"];

   MVP_Name    : constant VSS.Strings.Virtual_String := "mvp";
   Color_Name  : constant VSS.Strings.Virtual_String := "color";
   Center_Name : constant VSS.Strings.Virtual_String := "center";
   Radius_Name : constant VSS.Strings.Virtual_String := "radius";

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Circle_Program'Class) is
   begin
      Self.Add_Shader_From_Source_Code (OpenGL.Vertex, Vertex_Code);
      Self.Add_Shader_From_Source_Code (OpenGL.Geometry, Geometry_Code);
      Self.Add_Shader_From_Source_Code (OpenGL.Fragment, Fragment_Code);
   end Initialize;

   ----------
   -- Link --
   ----------

   overriding function Link (Self : in out Circle_Program) return Boolean is
   begin
      if not OpenGL.Programs.OpenGL_Program (Self).Link then
         return False;
      end if;

      Self.MVP    := Self.Uniform_Location (MVP_Name);
      Self.Color  := Self.Uniform_Location (Color_Name);
      Self.Center := Self.Attribute_Location (Center_Name);
      Self.Radius := Self.Attribute_Location (Radius_Name);

      return True;
   end Link;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Self : in out Circle_Program'Class;
      To   : OpenGL.GLubyte_Vector_3)
   is
      use type OpenGL.GLfloat;

      C : constant OpenGL.GLfloat_Vector_3 :=
        [for J in To'Range => OpenGL.GLfloat (To (J)) / 255.0];

   begin
      Self.Set_Uniform_Value (Self.Color, C);
   end Set_Color;

   -------------
   -- Set_MVP --
   -------------

   procedure Set_MVP
     (Self : in out Circle_Program'Class;
      MVP  : OpenGL.GLfloat_Matrix_4x4) is
   begin
      Self.Set_Uniform_Value (Self.MVP, MVP);
   end Set_MVP;

   ----------------------------
   -- Set_Vertex_Data_Buffer --
   ----------------------------

   procedure Set_Vertex_Data_Buffer
     (Self   : in out Circle_Program'Class;
      Buffer : in out Vertex_Data_Buffers.OpenGL_Buffer'Class)
   is
      Dummy : Vertex_Data;

   begin
      Buffer.Bind;

      Self.Enable_Attribute_Array (Self.Center);
      Self.Enable_Attribute_Array (Self.Radius);

      Self.Set_Attribute_Buffer
       (Location   => Self.Center,
        Data_Type  => OpenGL.GL_FLOAT,
        Tuple_Size => Dummy.Center'Length,
        Offset     => Dummy.Center'Position,
        Stride     => Vertex_Data_Buffers.Stride);
      Self.Set_Attribute_Buffer
       (Location   => Self.Radius,
        Data_Type  => OpenGL.GL_FLOAT,
        Tuple_Size => 1,
        Offset     => Dummy.Radius'Position,
        Stride     => Vertex_Data_Buffers.Stride);
   end Set_Vertex_Data_Buffer;

end GUI.Programs.Circles;
