--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Command_Line;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

with Gdk.GLContext;
with Glib.Application;
with Glib.Error;
with Gtk.Application;
with Gtk.Application_Window;
with Gtk.GLArea;
with Gtk.Widget;

--  with GL.Types;
--  with Orka.Rendering.Buffers;
--  with Orka.Rendering.Drawing;
--  --  with Orka.Rendering.Framebuffers;
--  with Orka.Rendering.Programs.Modules;
--  with Orka.Resources.Locations.Directories;

with OpenGL.Contexts;
--  with OpenGL.Generic_Buffers;
--  with OpenGL.Programs;
with epoxy;
with epoxy_gl_generated_h;

with Pyramid.Programs;

package body Telemetry.GUI.Main_Window is

   use type OpenGL.GLfloat;
   use type OpenGL.GLbitfield;

   App : Gtk.Application.Gtk_Application;
   AW  : Gtk.Application_Window.Gtk_Application_Window;
   GLA : Gtk.GLArea.Gtk_GLArea;

   View_Matrix     : OpenGL.GLfloat_Matrix_4x4 :=
     [[1.0, 0.0, 0.0, 0.0],
      [0.0, 1.0, 0.0, 0.0],
      [0.0, 0.0, 1.0, 0.0],
      [0.0, 0.0, 0.0, 1.0]];
   Viewport_Matrix : OpenGL.GLfloat_Matrix_4x4 :=
     [[1.0, 0.0, 0.0, 0.0],
      [0.0, 1.0, 0.0, 0.0],
      [0.0, 0.0, 1.0, 0.0],
      [0.0, 0.0, 0.0, 1.0]];

   Context : OpenGL.Contexts.OpenGL_Context;
   Program : Pyramid.Programs.Pyramid_Program;
   Buffer  : Pyramid.Programs.Vertex_Data_Buffers.OpenGL_Buffer
               (OpenGL.Vertex);
   --  Unit    : constant OpenGL.Texture_Unit := 0;

   Points : constant Pyramid.Programs.Vertex_Data_Array
     --  := [(VP => [0.0, 0.5, 0.0]),
     --      (VP => [0.5, -0.5, 0.0]),
     --      (VP => [-0.5, -0.5, 0.0])];
     := [([0.0, 0.5, 0.0],   [0.5, 1.0]),
         ([0.5, -0.5, 0.0],  [1.0, 0.0]),
         ([-0.5, -0.5, 0.0], [0.0, 0.0])];

   procedure On_Activate
     (Self : access Glib.Application.Gapplication_Record'Class);

   procedure On_Realize (Self : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Resize
     (Self   : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);

   function On_Render
     (Self      : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      GLContext : not null access Gdk.GLContext.Gdk_GLContext_Record'Class)
      return Boolean;

   --  procedure Setup;

   package Elementary_Function is
     new Ada.Numerics.Generic_Elementary_Functions (OpenGL.GLfloat);

   function Rotate_X (Angle : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4;

   function Rotate_Z (Angle : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4;

   function Degrees_To_Radians (Item : OpenGL.GLfloat) return OpenGL.GLfloat;

   --  Program     : Orka.Rendering.Programs.Program;
   --  Framebuffer : Orka.Rendering.Framebuffers.Framebuffer (True);

   ------------------------
   -- Degrees_To_Radians --
   ------------------------

   function Degrees_To_Radians (Item : OpenGL.GLfloat) return OpenGL.GLfloat is
   begin
      return Item / 180.0 * Ada.Numerics.Pi;
   end Degrees_To_Radians;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Status : Glib.Gint;

   begin
      App :=
        Gtk.Application.Gtk_Application_New
          (Flags => Glib.Application.G_Application_Flags_None);

      App.On_Activate (Call => On_Activate'Access);

      Status := App.Run;

      App.Unref;

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Status));
   end Initialize;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
     (Self : access Glib.Application.Gapplication_Record'Class) is
   begin
      AW :=
        Gtk.Application_Window.Gtk_Application_Window_New
          (Gtk.Application.Gtk_Application (Self));

      GLA := Gtk.GLArea.Gtk_GLArea_New;
      GLA.On_Realize (Call => On_Realize'Access);
      GLA.On_Resize (Call => On_Resize'Access);
      GLA.On_Render (Call => On_Render'Access);

      AW.Add (GLA);

      AW.Show_All;
   end On_Activate;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize (Self : access Gtk.Widget.Gtk_Widget_Record'Class) is
      use type OpenGL.GLenum;

      pragma Unreferenced (Self);
      VAO   : aliased epoxy.GLuint;
      Error : OpenGL.GLenum;

   begin
      Ada.Text_IO.Put_Line ("On_Realize");
      GLA.Make_Current;

      Context.Create (GLA.Get_Context);

      if epoxy_gl_generated_h.glGetError.all /= OpenGL.GL_NO_ERROR then
         raise Program_Error;
      end if;

      epoxy_gl_generated_h.glGenVertexArrays (1, VAO'Access);

      if epoxy_gl_generated_h.glGetError.all /= OpenGL.GL_NO_ERROR then
         raise Program_Error;
      end if;

      epoxy_gl_generated_h.glBindVertexArray (VAO);

      if epoxy_gl_generated_h.glGetError.all /= OpenGL.GL_NO_ERROR then
         raise Program_Error;
      end if;

      Buffer.Create;
      Buffer.Bind;
      Buffer.Allocate (Points);

      if epoxy_gl_generated_h.glGetError.all /= OpenGL.GL_NO_ERROR then
         raise Program_Error;
      end if;

      Program.Initialize;
      if epoxy_gl_generated_h.glGetError.all /= OpenGL.GL_NO_ERROR then
         raise Program_Error;
      end if;
      Program.Bind;
      if epoxy_gl_generated_h.glGetError.all /= OpenGL.GL_NO_ERROR then
         raise Program_Error;
      end if;
      Program.Set_Vertex_Data_Buffer (Buffer);
      --  Program.Set_Texture_Unit (Unit);

      Error := epoxy_gl_generated_h.glGetError.all;
      if Error /= OpenGL.GL_NO_ERROR then
         raise Program_Error with Error'Img;
      end if;
   end On_Realize;

   ---------------
   -- On_Render --
   ---------------

   function On_Render
     (Self      : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      GLContext : not null access Gdk.GLContext.Gdk_GLContext_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Self, GLContext);
      --  use type Orka.Float_32;
      use type Glib.Error.GError;
      use type OpenGL.GLfloat_Matrix_4x4;

      --  Vertices : constant Orka.Float_32_Array
      --    := (-0.5, -0.5, 0.0, 1.0,     1.0, 0.0, 0.0, 0.0,
      --         0.5, -0.5, 0.0, 1.0,     0.0, 1.0, 0.0, 0.0,
      --         0.0,  0.5, 0.0, 1.0,     0.0, 0.0, 1.0, 0.0);
      --
      --  Buffer  : Orka.Rendering.Buffers.Buffer :=
      --    Orka.Rendering.Buffers.Create_Buffer
      --      (Flags => (others => False),
      --       Data  => Vertices);

   begin
      Ada.Text_IO.Put_Line ("On_Render");

      View_Matrix :=
        Rotate_X (Degrees_To_Radians (70.0))
          * Rotate_Z (Degrees_To_Radians (45.0));

      --  Framebuffer.Set_Default_Values (Values => (others => <>));
      --  Framebuffer.Use_Framebuffer;
      --  Buffer.Bind
      --    (Target => Orka.Rendering.Buffers.Shader_Storage, Index => 0);
      --  Program.Use_Program;
      --
      --  Framebuffer.Clear ((Color => True, others => <>));
--
--        Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);

      Context.Functions.Enable (OpenGL.GL_DEPTH_TEST);
      Context.Functions.Clear_Color (0.2, 0.2, 0.2, 1.0);

      --

      --  Buffer.Bind;
      --  Buffer.Allocate (Points);

      Program.Bind;
      Program.Set_MVP (Viewport_Matrix * View_Matrix);
      --  Program.Set_MVP (View_Matrix * Viewport_Matrix);
      --  Program.Set_MVP (MVP);
      --  Program.Set_Vertex_Data_Buffer (Buffer);

      --

      Context.Functions.Clear
        (OpenGL.GL_DEPTH_BUFFER_BIT + OpenGL.GL_COLOR_BUFFER_BIT);

      Context.Functions.Draw_Arrays (OpenGL.GL_TRIANGLES, 0, Points'Length);

      if GLA.Get_Error /= null then
         Ada.Text_IO.Put_Line (Glib.Error.Get_Message (GLA.Get_Error));
      end if;

      return True;
   end On_Render;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
     (Self   : access Gtk.GLArea.Gtk_GLArea_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint)
   is
      pragma Unreferenced (Self, Width, Height);

      W : constant OpenGL.GLfloat := OpenGL.GLfloat (Width);
      H : constant OpenGL.GLfloat := OpenGL.GLfloat (Height);

   begin
      --  Setup;
      Ada.Text_IO.Put_Line ("On_Resize");

      if W < H then
         Viewport_Matrix (1, 1) := 1.0;
         Viewport_Matrix (2, 2) := W / H;

      else
         Viewport_Matrix (1, 1) := H / W;
         Viewport_Matrix (2, 2) := 1.0;
      end if;

      --  Framebuffer :=
      --    Orka.Rendering.Framebuffers.Create_Default_Framebuffer
      --      (Width => Integer (Width), Height => Integer (Height));
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

   --  -----------
   --  -- Setup --
   --  -----------
   --
   --  Initialized : Boolean := False;
   --
   --  procedure Setup is
   --     --  Shader_Location : Orka.Resources.Locations.Location_Ptr :=
   --     --  Orka.Resources.Locations.Directories.Create_Location ("shaders");
   --
   --     --  Program :
   --  begin
   --     if Initialized then
   --        return;
   --     end if;
   --
   --     Ada.Text_IO.Put_Line ("Setup");
   --
   --     Initialized := True;
   --
   --     --  GLA.Make_Current;
   --
   --     --  Program :=
   --     --    Orka.Rendering.Programs.Create_Program
   --     --      (Orka.Rendering.Programs.Modules.Create_Module
   --     --         (Location => Shader_Location,
   --     --          VS       => "ogl.vert",
   --     --          FS       => "ogl.frag"));
   --  end Setup;

end Telemetry.GUI.Main_Window;
