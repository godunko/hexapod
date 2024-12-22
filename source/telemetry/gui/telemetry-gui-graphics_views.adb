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

with Hexapod.Parameters.Control_Cycle;
with Kinematics.Configuration;
with Legs.Gait_Generator;
with Legs.State;
with Legs.Trajectory;
with Legs.Trajectory_Generator;
with Legs.Workspace;

package body Telemetry.GUI.Graphics_Views is

   --  XXX BEGIN stub transition

   use type OpenGL.GLfloat;

   Context : OpenGL.Contexts.OpenGL_Context;
   Buffer  : Telemetry.GUI.Programs.Lines.Vertex_Data_Buffers.OpenGL_Buffer
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
      Points : Telemetry.GUI.Programs.Lines.Vertex_Data_Array (1 .. 44);
      Last   : Natural := 0;

   begin
      for J in 0 .. 10 loop
         declare
            X : constant OpenGL.GLfloat :=
              -0.5 + (0.1 * OpenGL.GLfloat (J)) + Self.Grid_Offset_X;

         begin
            exit when X > 0.5;

            Last := @ + 1;
            Points (Last) := (VP => [X, -0.5, 0.0]);
            Last := @ + 1;
            Points (Last) := (VP => [X, 0.5, 0.0]);
         end;
      end loop;

      for J in 0 .. 10 loop
         declare
            Y : constant OpenGL.GLfloat :=
              -0.5 + (0.1 * OpenGL.GLfloat (J)) + Self.Grid_Offset_Y;

         begin
            exit when Y > 0.5;

            Last := @ + 1;
            Points (Last) := (VP => [-0.5, Y, 0.0]);
            Last := @ + 1;
            Points (Last) := (VP => [0.5, Y, 0.0]);
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
      Points : Telemetry.GUI.Programs.Lines.Vertex_Data_Array (1 .. 12 + 24);
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
      Legs.Gait_Generator.Set_Velocity (1.0, 0.0, 0.0);
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

      Self.Line_Program := new Telemetry.GUI.Programs.Lines.Line_Program;
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

      Legs.Trajectory_Generator.Tick;
      Legs.Gait_Generator.Tick;

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
   begin
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

end Telemetry.GUI.Graphics_Views;
