--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Numerics.Generic_Elementary_Functions;

with Gdk.GLContext;

with epoxy;
with epoxy_gl_generated_h;
with OpenGL.Contexts;

with Pyramid.Programs;

package body Telemetry.GUI.Graphics_Views is

   --  XXX BEGIN stub transition

   use type OpenGL.GLfloat;

   Context : OpenGL.Contexts.OpenGL_Context;
   Buffer  : Pyramid.Programs.Vertex_Data_Buffers.OpenGL_Buffer
               (OpenGL.Vertex);
   Program : Pyramid.Programs.Pyramid_Program;

   Points : constant Pyramid.Programs.Vertex_Data_Array
     --  := [(VP => [0.0, 0.5, 0.0]),
     --      (VP => [0.5, -0.5, 0.0]),
     --      (VP => [-0.5, -0.5, 0.0])];
     := [([0.0, 0.5, 0.0],   [0.5, 1.0]),
         ([0.5, -0.5, 0.0],  [1.0, 0.0]),
         ([-0.5, -0.5, 0.0], [0.0, 0.0])];

   package Elementary_Function is
     new Ada.Numerics.Generic_Elementary_Functions (OpenGL.GLfloat);

   function Rotate_X (Angle : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4;

   function Rotate_Z (Angle : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4;

   function Degrees_To_Radians (Item : OpenGL.GLfloat) return OpenGL.GLfloat;

   --  XXX END stub transition

   procedure Initialize (Self : not null access Graphics_View_Record'Class);

   procedure On_Realize (Self : in out Graphics_View_Record'Class);

   procedure On_Resize
     (Self   : in out Graphics_View_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);

   function On_Render
     (Self      : in out Graphics_View_Record'Class;
      GLContext : not null access Gdk.GLContext.Gdk_GLContext_Record'Class)
      return Boolean;

   procedure Dispatch_Realize
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Dispatch_Resize
     (Self   : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);

   function Dispatch_Render
     (Self      : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      GLContext : not null access Gdk.GLContext.Gdk_GLContext_Record'Class)
      return Boolean;

   ------------------------
   -- Degrees_To_Radians --
   ------------------------

   function Degrees_To_Radians (Item : OpenGL.GLfloat) return OpenGL.GLfloat is
   begin
      return Item / 180.0 * Ada.Numerics.Pi;
   end Degrees_To_Radians;

   ----------------------
   -- Dispatch_Realize --
   ----------------------

   procedure Dispatch_Realize
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Graphics_View_Record'Class (Self.all).On_Realize;
   end Dispatch_Realize;

   ---------------------
   -- Dispatch_Render --
   ---------------------

   function Dispatch_Render
     (Self      : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      GLContext : not null access Gdk.GLContext.Gdk_GLContext_Record'Class)
      return Boolean is
   begin
      return Graphics_View_Record'Class (Self.all).On_Render (GLContext);
   end Dispatch_Render;

   ---------------------
   -- Dispatch_Resize --
   ---------------------

   procedure Dispatch_Resize
     (Self   : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint) is
   begin
      Graphics_View_Record'Class (Self.all).On_Resize
        (Width => Width, Height => Height);
   end Dispatch_Resize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Graphics_View) is
   begin
      Self := new Graphics_View_Record;
      Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Graphics_View_Record'Class) is
   begin
      Gtk.GLArea.Initialize (Self);

      Self.On_Realize (Call => Dispatch_Realize'Access);
      Self.On_Resize (Call => Dispatch_Resize'Access);
      Self.On_Render (Call => Dispatch_Render'Access);

      Self.Viewport_Matrix :=
        [[1.0, 0.0, 0.0, 0.0],
         [0.0, 1.0, 0.0, 0.0],
         [0.0, 0.0, 1.0, 0.0],
         [0.0, 0.0, 0.0, 1.0]];
   end Initialize;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize (Self : in out Graphics_View_Record'Class) is
      VAO : aliased epoxy.GLuint;

   begin
      Self.Make_Current;

      Context.Create (Self.Get_Context);

      epoxy_gl_generated_h.glGenVertexArrays (1, VAO'Access);
      epoxy_gl_generated_h.glBindVertexArray (VAO);

      Buffer.Create;
      Buffer.Bind;

      Program.Initialize;
      Program.Bind;
      Program.Set_Vertex_Data_Buffer (Buffer);
   end On_Realize;

   ---------------
   -- On_Render --
   ---------------

   function On_Render
     (Self      : in out Graphics_View_Record'Class;
      GLContext : not null access Gdk.GLContext.Gdk_GLContext_Record'Class)
      return Boolean
   is
      pragma Unreferenced (GLContext);

      use type OpenGL.GLfloat_Matrix_4x4;
      use type OpenGL.GLbitfield;

      View_Matrix : OpenGL.GLfloat_Matrix_4x4 :=
        [[1.0, 0.0, 0.0, 0.0],
         [0.0, 1.0, 0.0, 0.0],
         [0.0, 0.0, 1.0, 0.0],
         [0.0, 0.0, 0.0, 1.0]];

   begin
      View_Matrix :=
        Rotate_X (Degrees_To_Radians (Self.Vertical_Angle))
          * Rotate_Z (Degrees_To_Radians (Self.Horizontal_Angle));

      Context.Functions.Enable (OpenGL.GL_DEPTH_TEST);
      Context.Functions.Clear_Color (0.2, 0.2, 0.2, 1.0);

      --

      Buffer.Bind;
      Buffer.Allocate (Points);

      Program.Bind;
      Program.Set_MVP (Self.Viewport_Matrix * View_Matrix);

      --

      Context.Functions.Clear
        (OpenGL.GL_DEPTH_BUFFER_BIT + OpenGL.GL_COLOR_BUFFER_BIT);

      Context.Functions.Draw_Arrays (OpenGL.GL_TRIANGLES, 0, Points'Length);

      return True;
   end On_Render;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
     (Self   : in out Graphics_View_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint)
   is
      W : constant OpenGL.GLfloat := OpenGL.GLfloat (Width);
      H : constant OpenGL.GLfloat := OpenGL.GLfloat (Height);

   begin
      if W < H then
         Self.Viewport_Matrix (1, 1) := 1.0;
         Self.Viewport_Matrix (2, 2) := W / H;

      else
         Self.Viewport_Matrix (1, 1) := H / W;
         Self.Viewport_Matrix (2, 2) := 1.0;
      end if;
   end On_Resize;

   --------------
   -- Rotate_X --
   --------------

   function Rotate_X
     (Angle : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4
   is
      C : constant OpenGL.GLfloat := Elementary_Function.Cos (Angle);
      S : constant OpenGL.GLfloat := Elementary_Function.Sin (Angle);

   begin
      return
        [[1.0, 0.0, 0.0, 0.0],
         [0.0, C,   -S,  0.0],
         [0.0, S,   C,   0.0],
         [0.0, 0.0, 0.0, 1.0]];
   end Rotate_X;

   --------------
   -- Rotate_Z --
   --------------

   function Rotate_Z
     (Angle : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4
   is
      C : constant OpenGL.GLfloat := Elementary_Function.Cos (Angle);
      S : constant OpenGL.GLfloat := Elementary_Function.Sin (Angle);

   begin
      return
        [[C,   -S,  0.0, 0.0],
         [S,   C,   0.0, 0.0],
         [0.0, 0.0, 1.0, 0.0],
         [0.0, 0.0, 0.0, 1.0]];
   end Rotate_Z;

   -----------------------------
   -- Set_Horizontal_Rotation --
   -----------------------------

   procedure Set_Horizontal_Rotation
     (Self  : in out Graphics_View_Record'Class;
      Angle : Glib.Gdouble) is
   begin
      Self.Horizontal_Angle := OpenGL.GLfloat (Angle);
      Self.Queue_Draw;
   end Set_Horizontal_Rotation;

   ---------------------------
   -- Set_Vertical_Rotation --
   ---------------------------

   procedure Set_Vertical_Rotation
     (Self  : in out Graphics_View_Record'Class;
      Angle : Glib.Gdouble) is
   begin
      Self.Vertical_Angle := OpenGL.GLfloat (Angle);
      Self.Queue_Draw;
   end Set_Vertical_Rotation;

end Telemetry.GUI.Graphics_Views;
