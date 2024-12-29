--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Numerics.Generic_Elementary_Functions;

with Gdk.GLContext;

with epoxy_gl_generated_h;
with OpenGL.Contexts;

with CGK.Primitives.Circles_2D;
with CGK.Primitives.Points_2D;
with CGK.Primitives.Points_3D;
with CGK.Primitives.Transformations_2D;
with CGK.Primitives.Vectors_3D;
with CGK.Reals;

with Legs;
with Simulation.Control_Loop;

package body GUI.Graphics_Views is

   --  XXX BEGIN stub transition

   use type OpenGL.GLfloat;

   Context : OpenGL.Contexts.OpenGL_Context;
   Buffer  : GUI.Programs.Lines.Vertex_Data_Buffers.OpenGL_Buffer
               (OpenGL.Vertex);

   package Elementary_Function is
     new Ada.Numerics.Generic_Elementary_Functions (OpenGL.GLfloat);

   function Scale (Scale : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4;

   function Rotate_X (Angle : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4;

   function Rotate_Z (Angle : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4;

   function Mirror_Z return OpenGL.GLfloat_Matrix_4x4;

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

   procedure Build_Grid (Self : in out Graphics_View_Record'Class);

   procedure Build_Robot (Self : in out Graphics_View_Record'Class);

   function As_GLfloat_Vector_3
     (Item : CGK.Primitives.Points_3D.Point_3D)
      return OpenGL.GLfloat_Vector_3;

   -------------------------
   -- As_GLfloat_Vector_3 --
   -------------------------

   function As_GLfloat_Vector_3
     (Item : CGK.Primitives.Points_3D.Point_3D)
      return OpenGL.GLfloat_Vector_3 is
   begin
      return
        [1 => OpenGL.GLfloat (CGK.Primitives.Points_3D.X (Item)),
         2 => OpenGL.GLfloat (CGK.Primitives.Points_3D.Y (Item)),
         3 => OpenGL.GLfloat (CGK.Primitives.Points_3D.Z (Item))];
   end As_GLfloat_Vector_3;

   ----------------
   -- Build_Grid --
   ----------------

   procedure Build_Grid (Self : in out Graphics_View_Record'Class) is

      use type CGK.Reals.Real;

      X1             : constant := -0.5;
      X2             : constant := 0.5;
      Y1             : constant := -0.5;
      Y2             : constant := 0.5;

      Points         : GUI.Programs.Lines.Vertex_Data_Array (1 .. 44);
      Last           : Natural := 0;
      Transformation : CGK.Primitives.Transformations_2D.Transformation_2D;

      procedure Append (Point : CGK.Primitives.Points_2D.Point_2D);

      ------------
      -- Append --
      ------------

      procedure Append (Point : CGK.Primitives.Points_2D.Point_2D) is
      begin
         Last := @ + 1;
         Points (Last) :=
           (VP =>
              [OpenGL.GLfloat (CGK.Primitives.Points_2D.X (Point)),
               OpenGL.GLfloat (CGK.Primitives.Points_2D.Y (Point)),
               0.0]);
      end Append;

   begin
      CGK.Primitives.Transformations_2D.Rotate
        (Transformation, Self.Scene.Ground_Rotate_Z);

      for J in 0 .. 10 loop
         declare
            X  : constant CGK.Reals.Real :=
              -0.5 + (0.1 * CGK.Reals.Real (J)) + Self.Scene.Ground_Offset_X;
            P1 : CGK.Primitives.Points_2D.Point_2D :=
              CGK.Primitives.Points_2D.Create_Point_2D (X, Y1);
            P2 : CGK.Primitives.Points_2D.Point_2D :=
              CGK.Primitives.Points_2D.Create_Point_2D (X, Y2);

         begin
            exit when X > 0.5;

            if X >= -0.5 then
               CGK.Primitives.Points_2D.Transform (P1, Transformation);
               CGK.Primitives.Points_2D.Transform (P2, Transformation);

               Append (P1);
               Append (P2);
            end if;
         end;
      end loop;

      for J in 0 .. 10 loop
         declare
            Y  : constant CGK.Reals.Real :=
              -0.5 + (0.1 * CGK.Reals.Real (J)) + Self.Scene.Ground_Offset_Y;
            P1 : CGK.Primitives.Points_2D.Point_2D :=
              CGK.Primitives.Points_2D.Create_Point_2D (X1, Y);
            P2 : CGK.Primitives.Points_2D.Point_2D :=
              CGK.Primitives.Points_2D.Create_Point_2D (X2, Y);

         begin
            exit when Y > 0.5;

            if Y >= -0.5 then
               CGK.Primitives.Points_2D.Transform (P1, Transformation);
               CGK.Primitives.Points_2D.Transform (P2, Transformation);

               Append (P1);
               Append (P2);
            end if;
         end;
      end loop;

      Self.Line_Elements := OpenGL.GLsizei (Last);

      Buffer.Bind;
      Buffer.Allocate (Points (Points'First .. Last));
   end Build_Grid;

   -----------------
   -- Build_Robot --
   -----------------

   procedure Build_Robot (Self : in out Graphics_View_Record'Class) is

      use type CGK.Primitives.Points_3D.Point_3D;

      Offset   : constant CGK.Primitives.Vectors_3D.Vector_3D :=
        CGK.Primitives.Vectors_3D.As_Vector_3D
          (0.0, 0.0, Self.Scene.Body_Height);
      Verteces : GUI.Programs.Lines.Vertex_Data_Array (1 .. 12 + 36);
      Last     : Natural := 0;

      procedure Append (Point : CGK.Primitives.Points_3D.Point_3D);

      procedure Build_Leg (Leg : Legs.Leg_Index);

      ------------
      -- Append --
      ------------

      procedure Append (Point : CGK.Primitives.Points_3D.Point_3D) is
      begin
         Last := @ + 1;
         Verteces (Last) :=
           (VP => As_GLfloat_Vector_3 (Point + Offset));
      end Append;

      ---------------
      -- Build_Leg --
      ---------------

      procedure Build_Leg (Leg : Legs.Leg_Index) is
      begin
         --  Coxa

         Append (Self.Scene.Legs (Leg).Joint_1);
         Append (Self.Scene.Legs (Leg).Joint_2);

         --  Femur

         Append (Self.Scene.Legs (Leg).Joint_2);
         Append (Self.Scene.Legs (Leg).Joint_3);

         --  Tibia

         Append (Self.Scene.Legs (Leg).Joint_3);
         Append (Self.Scene.Legs (Leg).Effector);
      end Build_Leg;

   begin
      for J in Legs.Leg_Index loop
         declare
            use type Legs.Leg_Index;

            Leg_1 : constant Legs.Leg_Index := J;
            Leg_2 : constant Legs.Leg_Index :=
              (if J /= Legs.Leg_Index'Last
                 then Legs.Leg_Index'Succ (J)
                 else Legs.Leg_Index'First);

         begin
            Append (Self.Scene.Legs (Leg_1).Joint_1);
            Append (Self.Scene.Legs (Leg_2).Joint_1);
         end;
      end loop;

      for J in Legs.Leg_Index loop
         Build_Leg (J);
      end loop;

      Self.Line_Elements := OpenGL.GLsizei (Last);

      Buffer.Bind;
      Buffer.Allocate (Verteces (Verteces'First .. Last));
   end Build_Robot;

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

      Self.Viewport_Matrix    :=
        [[1.0, 0.0, 0.0, 0.0],
         [0.0, 1.0, 0.0, 0.0],
         [0.0, 0.0, 1.0, 0.0],
         [0.0, 0.0, 0.0, 1.0]];

      Self.On_Realize (Call => Dispatch_Realize'Access);
      Self.On_Resize (Call => Dispatch_Resize'Access);
      Self.On_Render (Call => Dispatch_Render'Access);

      Self.Set_Has_Depth_Buffer (True);
   end Initialize;

   --------------
   -- Mirror_Z --
   --------------

   function Mirror_Z return OpenGL.GLfloat_Matrix_4x4 is
   begin
      return
        [[1.0, 0.0,  0.0, 0.0],
         [0.0, 1.1,  0.0, 0.0],
         [0.0, 0.0, -1.0, 0.0],
         [0.0, 0.0,  0.0, 1.0]];
   end Mirror_Z;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize (Self : in out Graphics_View_Record'Class) is
   begin
      Self.Make_Current;

      Context.Create (Self.Get_Context);

      epoxy_gl_generated_h.glGenVertexArrays (1, Self.Line_VAO'Access);
      epoxy_gl_generated_h.glBindVertexArray (Self.Line_VAO);

      Buffer.Create;
      Buffer.Bind;

      Self.Line_Program := new GUI.Programs.Lines.Line_Program;
      Self.Line_Program.Initialize;
      Self.Line_Program.Bind;
      Self.Line_Program.Set_Vertex_Data_Buffer (Buffer);

      epoxy_gl_generated_h.glGenVertexArrays (1, Self.Circle_VAO'Access);
      epoxy_gl_generated_h.glBindVertexArray (Self.Circle_VAO);

      Self.Circle_Buffer :=
        new GUI.Programs.Circles.Vertex_Data_Buffers.OpenGL_Buffer
              (OpenGL.Vertex);
      Self.Circle_Buffer.Create;
      Self.Circle_Buffer.Bind;

      Self.Circle_Program := new GUI.Programs.Circles.Circle_Program;
      Self.Circle_Program.Initialize;
      Self.Circle_Program.Bind;
      Self.Circle_Program.Set_Vertex_Data_Buffer (Self.Circle_Buffer.all);
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

      View_Matrix : OpenGL.GLfloat_Matrix_4x4;

   begin
      Self.Scene := Simulation.Control_Loop.Get_Scene;

      View_Matrix :=
        Scale (Self.Scale)
          * Mirror_Z
          * Rotate_X (Degrees_To_Radians (Self.Vertical_Angle))
          * Rotate_Z (Degrees_To_Radians (Self.Horizontal_Angle));

      Context.Functions.Enable (OpenGL.GL_DEPTH_TEST);
      Context.Functions.Depth_Func (OpenGL.GL_LESS);
      Context.Functions.Clear_Color (0.2, 0.2, 0.2, 1.0);

      Context.Functions.Clear
        (OpenGL.GL_DEPTH_BUFFER_BIT + OpenGL.GL_COLOR_BUFFER_BIT);

      --  Draw ground grid and robot

      epoxy_gl_generated_h.glBindVertexArray (Self.Line_VAO);

      Self.Line_Program.Bind;
      Self.Line_Program.Set_MVP (Self.Viewport_Matrix * View_Matrix);

      Self.Build_Grid;
      Self.Line_Program.Set_Color ([0, 0, 255]);
      Context.Functions.Draw_Arrays (OpenGL.GL_LINES, 0, Self.Line_Elements);

      Self.Build_Robot;
      Self.Line_Program.Set_Color ([0, 255, 0]);
      Context.Functions.Draw_Arrays (OpenGL.GL_LINES, 0, Self.Line_Elements);

      --  Draw workspaces

      epoxy_gl_generated_h.glBindVertexArray (Self.Circle_VAO);

      declare
         Circles : GUI.Programs.Circles.Vertex_Data_Array (1 .. 6);

      begin
         for J in Legs.Leg_Index loop
            declare
               Circle : constant CGK.Primitives.Circles_2D.Circle_2D :=
                 Self.Scene.Legs (J).Workspace;

            begin
               Circles (Legs.Leg_Index'Pos (J) + 1) :=
           (Center =>
              [OpenGL.GLfloat
                 (CGK.Primitives.Points_2D.X
                    (CGK.Primitives.Circles_2D.Center
                       (Circle))),
               OpenGL.GLfloat
                 (CGK.Primitives.Points_2D.Y
                    (CGK.Primitives.Circles_2D.Center
                       (Circle)))],
            Radius =>
              OpenGL.GLfloat
                (CGK.Primitives.Circles_2D.Radius
                     (Circle)));
            end;
         end loop;

         Self.Circle_Buffer.Bind;
         Self.Circle_Buffer.Allocate (Circles);
      end;

      Self.Circle_Program.Bind;
      Self.Circle_Program.Set_MVP (Self.Viewport_Matrix * View_Matrix);
      Self.Circle_Program.Set_Color ([63, 0, 0]);
      Context.Functions.Draw_Arrays (OpenGL.GL_POINTS, 0, 6);

      Self.Queue_Draw;
      --  Request redraw of the scene.

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
         Self.Viewport_Matrix (1, 1) := H / W;
         Self.Viewport_Matrix (2, 2) := 1.0;

      else
         Self.Viewport_Matrix (1, 1) := 1.0;
         Self.Viewport_Matrix (2, 2) := W / H;
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

   -----------
   -- Scale --
   -----------

   function Scale (Scale : OpenGL.GLfloat) return OpenGL.GLfloat_Matrix_4x4 is
   begin
      return
        [[Scale, 0.0,   0.0,   0.0],
         [0.0,   Scale, 0.0,   0.0],
         [0.0,   0.0,   Scale, 0.0],
         [0.0,   0.0,   0.0,   1.0]];
   end Scale;

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

   ---------------
   -- Set_Scale --
   ---------------

   procedure Set_Scale
     (Self : in out Graphics_View_Record'Class;
      To   : Glib.Gdouble) is
   begin
      Self.Scale := OpenGL.GLfloat (To);
      Self.Queue_Draw;
   end Set_Scale;

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

end GUI.Graphics_Views;
