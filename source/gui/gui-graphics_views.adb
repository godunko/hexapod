--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Numerics.Generic_Elementary_Functions;

with Gdk.GLContext;
with Glib.Main;

with epoxy;
with epoxy_gl_generated_h;
with OpenGL.Contexts;

with CGK.Primitives.Points_2D;
with CGK.Primitives.Transformations_2D;
with CGK.Primitives.Vectors_2D;
with CGK.Reals.Elementary_Functions;

with Hexapod.Parameters.Control_Cycle;
with Kinematics.Configuration;
with Legs.Gait_Generator;
with Legs.State;
with Legs.Trajectory;
with Legs.Trajectory_Generator;
with Legs.Workspace;

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

   function On_Timeout (Data : Graphics_View) return Boolean;

   package Sources is new Glib.Main.Generic_Sources (Graphics_View);

   procedure Build_Grid (Self : in out Graphics_View_Record'Class);

   procedure Build_Robot (Self : in out Graphics_View_Record'Class);

   package Movement is

      Body_Height : constant := 0.070;

      procedure Initialize;

      --  procedure Step;

   end Movement;

   --------------
   -- Movement --
   --------------

   package body Movement is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         Legs.Initialize;
         Legs.Workspace.Compute (Body_Height);
         Legs.Trajectory.Initialize;
         Legs.Trajectory_Generator.Initialize;
         Legs.Gait_Generator.Initialize;
      end Initialize;

   end Movement;

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
        (Transformation, Self.Ground_Rotate_Z);

      for J in 0 .. 10 loop
         declare
            X  : constant CGK.Reals.Real :=
              -0.5 + (0.1 * CGK.Reals.Real (J)) + Self.Ground_Offset_X;
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
              -0.5 + (0.1 * CGK.Reals.Real (J)) + Self.Ground_Offset_Y;
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
      Points : GUI.Programs.Lines.Vertex_Data_Array (1 .. 12 + 24);
      Last   : Natural := 0;

      procedure Build_Leg
        (Base_X   : OpenGL.GLfloat;
         Base_Y   : OpenGL.GLfloat;
         Base_Z   : OpenGL.GLfloat;
         Position : Kinematics.Position);

      ---------------
      -- Build_Leg --
      ---------------

      procedure Build_Leg
        (Base_X   : OpenGL.GLfloat;
         Base_Y   : OpenGL.GLfloat;
         Base_Z   : OpenGL.GLfloat;
         Position : Kinematics.Position) is
      begin
         Last := @ + 1;
         Points (Last) :=
           (VP =>
              [Base_X,
               Base_Y,
               Base_Z + Movement.Body_Height]);
         Last := @ + 1;
         Points (Last) :=
           (VP =>
              [OpenGL.GLfloat (Kinematics.X (Position)),
               OpenGL.GLfloat (Kinematics.Y (Position)),
               OpenGL.GLfloat (Kinematics.Z (Position))
                 + Movement.Body_Height]);
      end Build_Leg;

   begin
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LF_Base_X,
            Kinematics.Configuration.LF_Base_Y,
            Kinematics.Configuration.LF_Base_Z + Movement.Body_Height]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LM_Base_X,
            Kinematics.Configuration.LM_Base_Y,
            Kinematics.Configuration.LM_Base_Z + Movement.Body_Height]);

      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LM_Base_X,
            Kinematics.Configuration.LM_Base_Y,
            Kinematics.Configuration.LM_Base_Z + Movement.Body_Height]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LH_Base_X,
            Kinematics.Configuration.LH_Base_Y,
            Kinematics.Configuration.LH_Base_Z + Movement.Body_Height]);

      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LH_Base_X,
            Kinematics.Configuration.LH_Base_Y,
            Kinematics.Configuration.LH_Base_Z + Movement.Body_Height]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RH_Base_X,
            Kinematics.Configuration.RH_Base_Y,
            Kinematics.Configuration.RH_Base_Z + Movement.Body_Height]);

      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RH_Base_X,
            Kinematics.Configuration.RH_Base_Y,
            Kinematics.Configuration.RH_Base_Z + Movement.Body_Height]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RM_Base_X,
            Kinematics.Configuration.RM_Base_Y,
            Kinematics.Configuration.RM_Base_Z + Movement.Body_Height]);

      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RM_Base_X,
            Kinematics.Configuration.RM_Base_Y,
            Kinematics.Configuration.RM_Base_Z + Movement.Body_Height]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RF_Base_X,
            Kinematics.Configuration.RF_Base_Y,
            Kinematics.Configuration.RF_Base_Z + Movement.Body_Height]);

      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RF_Base_X,
            Kinematics.Configuration.RF_Base_Y,
            Kinematics.Configuration.RF_Base_Z + Movement.Body_Height]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LF_Base_X,
            Kinematics.Configuration.LF_Base_Y,
            Kinematics.Configuration.LF_Base_Z + Movement.Body_Height]);

      Build_Leg
        (Kinematics.Configuration.LF_Base_X,
         Kinematics.Configuration.LF_Base_Y,
         Kinematics.Configuration.LF_Base_Z,
         Legs.State.Position (Legs.Left_Front));
      Build_Leg
        (Kinematics.Configuration.LM_Base_X,
         Kinematics.Configuration.LM_Base_Y,
         Kinematics.Configuration.LM_Base_Z,
         Legs.State.Position (Legs.Left_Middle));
      Build_Leg
        (Kinematics.Configuration.LH_Base_X,
         Kinematics.Configuration.LH_Base_Y,
         Kinematics.Configuration.LH_Base_Z,
         Legs.State.Position (Legs.Left_Hind));

      Build_Leg
        (Kinematics.Configuration.RF_Base_X,
         Kinematics.Configuration.RF_Base_Y,
         Kinematics.Configuration.RF_Base_Z,
         Legs.State.Position (Legs.Right_Front));
      Build_Leg
        (Kinematics.Configuration.RM_Base_X,
         Kinematics.Configuration.RM_Base_Y,
         Kinematics.Configuration.RM_Base_Z,
         Legs.State.Position (Legs.Right_Middle));
      Build_Leg
        (Kinematics.Configuration.RH_Base_X,
         Kinematics.Configuration.RH_Base_Y,
         Kinematics.Configuration.RH_Base_Z,
         Legs.State.Position (Legs.Right_Hind));

      Self.Line_Elements := OpenGL.GLsizei (Last);

      Buffer.Bind;
      Buffer.Allocate (Points (Points'First .. Last));
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

      Self.On_Realize (Call => Dispatch_Realize'Access);
      Self.On_Resize (Call => Dispatch_Resize'Access);
      Self.On_Render (Call => Dispatch_Render'Access);

      Self.Viewport_Matrix :=
        [[1.0, 0.0, 0.0, 0.0],
         [0.0, 1.0, 0.0, 0.0],
         [0.0, 0.0, 1.0, 0.0],
         [0.0, 0.0, 0.0, 1.0]];

      Movement.Initialize;
      Legs.Gait_Generator.Set_Velocity (1.0, 1.0, 0.5);
   end Initialize;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize (Self : in out Graphics_View_Record'Class) is
      VAO : aliased epoxy.GLuint;
      SI  : Glib.Main.G_Source_Id;

   begin
      Self.Make_Current;

      Context.Create (Self.Get_Context);

      epoxy_gl_generated_h.glGenVertexArrays (1, VAO'Access);
      epoxy_gl_generated_h.glBindVertexArray (VAO);

      Buffer.Create;
      Buffer.Bind;

      Self.Line_Program := new GUI.Programs.Lines.Line_Program;
      Self.Line_Program.Initialize;
      Self.Line_Program.Bind;
      Self.Line_Program.Set_Vertex_Data_Buffer (Buffer);

      SI :=
        Sources.Timeout_Add
          (Interval =>
             Glib.Guint
               (Hexapod.Parameters.Control_Cycle.Tick_Duration * 1_000),
           Func     => On_Timeout'Access,
           Data     => Self'Unchecked_Access);
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
        Scale (Self.Scale)
          * Rotate_X (Degrees_To_Radians (Self.Vertical_Angle))
          * Rotate_Z (Degrees_To_Radians (Self.Horizontal_Angle));

      Context.Functions.Enable (OpenGL.GL_DEPTH_TEST);
      Context.Functions.Clear_Color (0.2, 0.2, 0.2, 1.0);

      Context.Functions.Clear
        (OpenGL.GL_DEPTH_BUFFER_BIT + OpenGL.GL_COLOR_BUFFER_BIT);

      Self.Line_Program.Bind;
      Self.Line_Program.Set_MVP (Self.Viewport_Matrix * View_Matrix);

      Self.Build_Grid;
      Self.Line_Program.Set_Color ([0, 0, 255]);
      Context.Functions.Draw_Arrays (OpenGL.GL_LINES, 0, Self.Line_Elements);

      Self.Build_Robot;
      Self.Line_Program.Set_Color ([0, 255, 0]);
      Context.Functions.Draw_Arrays (OpenGL.GL_LINES, 0, Self.Line_Elements);

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

   ----------------
   -- On_Timeout --
   ----------------

   function On_Timeout (Data : Graphics_View) return Boolean is
      use type CGK.Reals.Real;

   begin
      Legs.Trajectory_Generator.Tick;
      Legs.Gait_Generator.Tick;

      declare
         P0              : CGK.Primitives.Points_2D.Point_2D :=
           CGK.Primitives.Points_2D.Create_Point_2D (X => 0.0, Y => 0.0);
         P1              : CGK.Primitives.Points_2D.Point_2D :=
           CGK.Primitives.Points_2D.Create_Point_2D (X => 1.0, Y => 0.0);
         V               : CGK.Primitives.Vectors_2D.Vector_2D;
         Transformation  : CGK.Primitives.Transformations_2D.Transformation_2D;

      begin
         --  Compute rotation vector

         Legs.Trajectory.Transform
           (Legs.Trajectory_Generator.Trajectory.all, P0);
         Legs.Trajectory.Transform
           (Legs.Trajectory_Generator.Trajectory.all, P1);

         V := CGK.Primitives.Vectors_2D.Create_Vector_2D (P0, P1);

         Data.Ground_Rotate_Z :=
           @ + CGK.Reals.Elementary_Functions.Arctan
                 (X => CGK.Primitives.Vectors_2D.X (V),
                  Y => CGK.Primitives.Vectors_2D.Y (V));

         --  Compute coordinates offset

         CGK.Primitives.Transformations_2D.Set_Identity (Transformation);
         CGK.Primitives.Transformations_2D.Rotate
           (Transformation, -Data.Ground_Rotate_Z);
         CGK.Primitives.Points_2D.Transform (P0, Transformation);

         Data.Ground_Offset_X := @ + CGK.Primitives.Points_2D.X (P0);
         Data.Ground_Offset_Y := @ + CGK.Primitives.Points_2D.Y (P0);

         if Data.Ground_Offset_X < -0.1 then
            Data.Ground_Offset_X := @ + 0.1;

         elsif Data.Ground_Offset_X > 0.1 then
            Data.Ground_Offset_X := @ - 0.1;
         end if;

         if Data.Ground_Offset_Y < -0.1 then
            Data.Ground_Offset_Y := @ + 0.1;

         elsif Data.Ground_Offset_Y > 0.1 then
            Data.Ground_Offset_Y := @ - 0.1;
         end if;

      end;

      Data.Queue_Draw;

      return True;
   end On_Timeout;

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
