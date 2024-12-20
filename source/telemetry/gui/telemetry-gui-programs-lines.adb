--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with League.Characters.Latin;
with League.Strings;

package body Telemetry.GUI.Programs.Lines is

   use type League.Strings.Universal_String;

   Text_V : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String
       ("#version 130") & League.Characters.Latin.Line_Feed &
     "uniform mat4 mvp;" &
     "in vec3 vp;" &
     "void main() {" &
     "  gl_Position = mvp * vec4(vp, 1.0);" &
     "}";
   Text_F : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String
       ("#version 130") & League.Characters.Latin.Line_Feed &
     "out vec4 frag_colour;" &
     "void main() {" &
     "  frag_colour = vec4(0.0, 0.0, 0.8, 1.0);" &
     "}";

   MVP_Name : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("mvp");
   VP_Name  : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("vp");

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
      Self.VP  := Self.Attribute_Location (VP_Name);

      return True;
   end Link;

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

end Telemetry.GUI.Programs.Lines;
