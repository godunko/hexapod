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

with CGK.Primitives.Points_2D;
with CGK.Primitives.Transformations_2D;
with CGK.Reals;

with Kinematics.Configuration.Derived;
with Legs;
with Reals;
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

   --  function On_Timeout (Data : Graphics_View) return Boolean;

   --  package Sources is new Glib.Main.Generic_Sources (Graphics_View);

   procedure Build_Grid (Self : in out Graphics_View_Record'Class);

   procedure Build_Robot (Self : in out Graphics_View_Record'Class);

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

      use type CGK.Reals.Real;

      Points : GUI.Programs.Lines.Vertex_Data_Array (1 .. 12 + 36);
      Last   : Natural := 0;

      procedure Build_Leg
        (Base_X     : CGK.Reals.Real;
         Base_Y     : CGK.Reals.Real;
         Base_Z     : CGK.Reals.Real;
         Base_Gamma_Sin : CGK.Reals.Real;
         Base_Gamma_Cos : CGK.Reals.Real;
         d_1        : CGK.Reals.Real;
         r_1        : CGK.Reals.Real;
         α_1        : CGK.Reals.Real;
         d_2        : CGK.Reals.Real;
         r_2        : CGK.Reals.Real;
         α_2        : CGK.Reals.Real;
         d_3        : CGK.Reals.Real;
         r_3        : CGK.Reals.Real;
         α_3        : CGK.Reals.Real;
         Posture    : Kinematics.Posture);

      ---------------
      -- Build_Leg --
      ---------------

      procedure Build_Leg
        (Base_X     : CGK.Reals.Real;
         Base_Y     : CGK.Reals.Real;
         Base_Z     : CGK.Reals.Real;
         Base_Gamma_Sin : CGK.Reals.Real;
         Base_Gamma_Cos : CGK.Reals.Real;
         d_1        : CGK.Reals.Real;
         r_1        : CGK.Reals.Real;
         α_1        : CGK.Reals.Real;
         d_2        : CGK.Reals.Real;
         r_2        : CGK.Reals.Real;
         α_2        : CGK.Reals.Real;
         d_3        : CGK.Reals.Real;
         r_3        : CGK.Reals.Real;
         α_3        : CGK.Reals.Real;
         Posture    : Kinematics.Posture)
      is
         use type Reals.Transformations_3D.Transformation_3D;

         Origin           : constant Reals.Vectors_3D.Vector_3D :=
           Reals.Vectors_3D.To_Vector_3D (0.0, 0.0, 0.0);
         Point            : Reals.Vectors_3D.Vector_3D :=
           Reals.Vectors_3D.To_Vector_3D (0.0, 0.0, 0.0);

         Transformation   : Reals.Transformations_3D.Transformation_3D;
         Transformation_0 : Reals.Transformations_3D.Transformation_3D;
         Transformation_1 : constant
           Reals.Transformations_3D.Transformation_3D :=
             Reals.Transformation_3D_HD_Constructors.Create_Transformation_3D
               (d => d_1, θ => Kinematics.Theta_1 (Posture), r => r_1, α => α_1);
         Transformation_2 : constant
           Reals.Transformations_3D.Transformation_3D :=
             Reals.Transformation_3D_HD_Constructors.Create_Transformation_3D
               (d => d_2, θ => Kinematics.Theta_2 (Posture), r => r_2, α => α_2);
         Transformation_3 : constant
           Reals.Transformations_3D.Transformation_3D :=
             Reals.Transformation_3D_HD_Constructors.Create_Transformation_3D
               (d => d_3, θ => Kinematics.Theta_3 (Posture), r => r_3, α => α_3);

      begin
         Reals.Transformations_3D.Initialize
           (Self => Transformation_0,
            M_11 => Base_Gamma_Cos,
            M_12 => -Base_Gamma_Sin,
            M_13 => 0.0,
            M_14 => Base_X,
            M_21 => Base_Gamma_Sin,
            M_22 => Base_Gamma_Cos,
            M_23 => 0.0,
            M_24 => Base_Y,
            M_31 => 0.0,
            M_32 => 0.0,
            M_33 => 1.0,
            M_34 => Base_Z);

         Transformation := Transformation_0;
         Point := Reals.Transformations_3D.Transform (Transformation, Origin);

         --  Coxa

         Last := @ + 1;
         Points (Last) :=
           (VP =>
              [OpenGL.GLfloat (Reals.Vectors_3D.X (Point)),
               OpenGL.GLfloat (Reals.Vectors_3D.Y (Point)),
               OpenGL.GLfloat
                 (Reals.Vectors_3D.Z (Point) + Self.Scene.Body_Height)]);

         Transformation := Transformation * Transformation_1;
         Point := Reals.Transformations_3D.Transform (Transformation, Origin);

         Last := @ + 1;
         Points (Last) :=
           (VP =>
              [OpenGL.GLfloat (Reals.Vectors_3D.X (Point)),
               OpenGL.GLfloat (Reals.Vectors_3D.Y (Point)),
               OpenGL.GLfloat
                 (Reals.Vectors_3D.Z (Point) + Self.Scene.Body_Height)]);

         --  Femur

         Last := @ + 1;
         Points (Last) :=
           (VP =>
              [OpenGL.GLfloat (Reals.Vectors_3D.X (Point)),
               OpenGL.GLfloat (Reals.Vectors_3D.Y (Point)),
               OpenGL.GLfloat
                 (Reals.Vectors_3D.Z (Point) + Self.Scene.Body_Height)]);

         Transformation := Transformation * Transformation_2;
         Point := Reals.Transformations_3D.Transform (Transformation, Origin);

         Last := @ + 1;
         Points (Last) :=
           (VP =>
              [OpenGL.GLfloat (Reals.Vectors_3D.X (Point)),
               OpenGL.GLfloat (Reals.Vectors_3D.Y (Point)),
               OpenGL.GLfloat
                 (Reals.Vectors_3D.Z (Point) + Self.Scene.Body_Height)]);

         --  Tibia

         Last := @ + 1;
         Points (Last) :=
           (VP =>
              [OpenGL.GLfloat (Reals.Vectors_3D.X (Point)),
               OpenGL.GLfloat (Reals.Vectors_3D.Y (Point)),
               OpenGL.GLfloat
                 (Reals.Vectors_3D.Z (Point) + Self.Scene.Body_Height)]);

         Transformation := Transformation * Transformation_3;
         Point := Reals.Transformations_3D.Transform (Transformation, Origin);

         Last := @ + 1;
         Points (Last) :=
           (VP =>
              [OpenGL.GLfloat (Reals.Vectors_3D.X (Point)),
               OpenGL.GLfloat (Reals.Vectors_3D.Y (Point)),
               OpenGL.GLfloat
                 (Reals.Vectors_3D.Z (Point) + Self.Scene.Body_Height)]);
      end Build_Leg;

   begin
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LF_Base_X,
            Kinematics.Configuration.LF_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.LF_Base_Z + Self.Scene.Body_Height)]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LM_Base_X,
            Kinematics.Configuration.LM_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.LM_Base_Z + Self.Scene.Body_Height)]);

      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LM_Base_X,
            Kinematics.Configuration.LM_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.LM_Base_Z + Self.Scene.Body_Height)]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LH_Base_X,
            Kinematics.Configuration.LH_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.LH_Base_Z + Self.Scene.Body_Height)]);

      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LH_Base_X,
            Kinematics.Configuration.LH_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.LH_Base_Z + Self.Scene.Body_Height)]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RH_Base_X,
            Kinematics.Configuration.RH_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.RH_Base_Z + Self.Scene.Body_Height)]);

      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RH_Base_X,
            Kinematics.Configuration.RH_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.RH_Base_Z + Self.Scene.Body_Height)]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RM_Base_X,
            Kinematics.Configuration.RM_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.RM_Base_Z + Self.Scene.Body_Height)]);

      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RM_Base_X,
            Kinematics.Configuration.RM_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.RM_Base_Z + Self.Scene.Body_Height)]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RF_Base_X,
            Kinematics.Configuration.RF_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.RF_Base_Z + Self.Scene.Body_Height)]);

      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.RF_Base_X,
            Kinematics.Configuration.RF_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.RF_Base_Z + Self.Scene.Body_Height)]);
      Last := @ + 1;
      Points (Last) :=
        (VP =>
           [Kinematics.Configuration.LF_Base_X,
            Kinematics.Configuration.LF_Base_Y,
            OpenGL.GLfloat
              (Kinematics.Configuration.LF_Base_Z + Self.Scene.Body_Height)]);

      Build_Leg
        (Base_X         => Kinematics.Configuration.LF_Base_X,
         Base_Y         => Kinematics.Configuration.LF_Base_Y,
         Base_Z         => Kinematics.Configuration.LF_Base_Z,
         Base_Gamma_Sin => Kinematics.Configuration.Derived.LF_Sin_Gamma_0,
         Base_Gamma_Cos => Kinematics.Configuration.Derived.LF_Cos_Gamma_0,
         d_1            => Kinematics.Configuration.LF_DH_D1,
         r_1            => Kinematics.Configuration.LF_DH_R1,
         α_1            => Kinematics.Configuration.LF_DH_Alpha1,
         d_2            => Kinematics.Configuration.LF_DH_D2,
         r_2            => Kinematics.Configuration.LF_DH_R2,
         α_2            => Kinematics.Configuration.LF_DH_Alpha2,
         d_3            => Kinematics.Configuration.LF_DH_D3,
         r_3            => Kinematics.Configuration.LF_DH_R3,
         α_3            => Kinematics.Configuration.LF_DH_Alpha3,
         Posture        => Self.Scene.Legs_Posture (Legs.Left_Front));
      Build_Leg
        (Base_X         => Kinematics.Configuration.LM_Base_X,
         Base_Y         => Kinematics.Configuration.LM_Base_Y,
         Base_Z         => Kinematics.Configuration.LM_Base_Z,
         Base_Gamma_Sin => Kinematics.Configuration.Derived.LM_Sin_Gamma_0,
         Base_Gamma_Cos => Kinematics.Configuration.Derived.LM_Cos_Gamma_0,
         d_1            => Kinematics.Configuration.LM_DH_D1,
         r_1            => Kinematics.Configuration.LM_DH_R1,
         α_1            => Kinematics.Configuration.LM_DH_Alpha1,
         d_2            => Kinematics.Configuration.LM_DH_D2,
         r_2            => Kinematics.Configuration.LM_DH_R2,
         α_2            => Kinematics.Configuration.LM_DH_Alpha2,
         d_3            => Kinematics.Configuration.LM_DH_D3,
         r_3            => Kinematics.Configuration.LM_DH_R3,
         α_3            => Kinematics.Configuration.LM_DH_Alpha3,
         Posture        => Self.Scene.Legs_Posture (Legs.Left_Middle));
      Build_Leg
        (Base_X         => Kinematics.Configuration.LH_Base_X,
         Base_Y         => Kinematics.Configuration.LH_Base_Y,
         Base_Z         => Kinematics.Configuration.LH_Base_Z,
         Base_Gamma_Sin => Kinematics.Configuration.Derived.LH_Sin_Gamma_0,
         Base_Gamma_Cos => Kinematics.Configuration.Derived.LH_Cos_Gamma_0,
         d_1            => Kinematics.Configuration.LH_DH_D1,
         r_1            => Kinematics.Configuration.LH_DH_R1,
         α_1            => Kinematics.Configuration.LH_DH_Alpha1,
         d_2            => Kinematics.Configuration.LH_DH_D2,
         r_2            => Kinematics.Configuration.LH_DH_R2,
         α_2            => Kinematics.Configuration.LH_DH_Alpha2,
         d_3            => Kinematics.Configuration.LH_DH_D3,
         r_3            => Kinematics.Configuration.LH_DH_R3,
         α_3            => Kinematics.Configuration.LH_DH_Alpha3,
         Posture        => Self.Scene.Legs_Posture (Legs.Left_Hind));

      Build_Leg
        (Base_X         => Kinematics.Configuration.RF_Base_X,
         Base_Y         => Kinematics.Configuration.RF_Base_Y,
         Base_Z         => Kinematics.Configuration.RF_Base_Z,
         Base_Gamma_Sin => Kinematics.Configuration.Derived.RF_Sin_Gamma_0,
         Base_Gamma_Cos => Kinematics.Configuration.Derived.RF_Cos_Gamma_0,
         d_1            => Kinematics.Configuration.RF_DH_D1,
         r_1            => Kinematics.Configuration.RF_DH_R1,
         α_1            => Kinematics.Configuration.RF_DH_Alpha1,
         d_2            => Kinematics.Configuration.RF_DH_D2,
         r_2            => Kinematics.Configuration.RF_DH_R2,
         α_2            => Kinematics.Configuration.RF_DH_Alpha2,
         d_3            => Kinematics.Configuration.RF_DH_D3,
         r_3            => Kinematics.Configuration.RF_DH_R3,
         α_3            => Kinematics.Configuration.RF_DH_Alpha3,
         Posture        => Self.Scene.Legs_Posture (Legs.Right_Front));
      Build_Leg
        (Base_X         => Kinematics.Configuration.RM_Base_X,
         Base_Y         => Kinematics.Configuration.RM_Base_Y,
         Base_Z         => Kinematics.Configuration.RM_Base_Z,
         Base_Gamma_Sin => Kinematics.Configuration.Derived.RM_Sin_Gamma_0,
         Base_Gamma_Cos => Kinematics.Configuration.Derived.RM_Cos_Gamma_0,
         d_1            => Kinematics.Configuration.RM_DH_D1,
         r_1            => Kinematics.Configuration.RM_DH_R1,
         α_1            => Kinematics.Configuration.RM_DH_Alpha1,
         d_2            => Kinematics.Configuration.RM_DH_D2,
         r_2            => Kinematics.Configuration.RM_DH_R2,
         α_2            => Kinematics.Configuration.RM_DH_Alpha2,
         d_3            => Kinematics.Configuration.RM_DH_D3,
         r_3            => Kinematics.Configuration.RM_DH_R3,
         α_3            => Kinematics.Configuration.RM_DH_Alpha3,
         Posture        => Self.Scene.Legs_Posture (Legs.Right_Middle));
      Build_Leg
        (Base_X         => Kinematics.Configuration.RH_Base_X,
         Base_Y         => Kinematics.Configuration.RH_Base_Y,
         Base_Z         => Kinematics.Configuration.RH_Base_Z,
         Base_Gamma_Sin => Kinematics.Configuration.Derived.RH_Sin_Gamma_0,
         Base_Gamma_Cos => Kinematics.Configuration.Derived.RH_Cos_Gamma_0,
         d_1            => Kinematics.Configuration.RH_DH_D1,
         r_1            => Kinematics.Configuration.RH_DH_R1,
         α_1            => Kinematics.Configuration.RH_DH_Alpha1,
         d_2            => Kinematics.Configuration.RH_DH_D2,
         r_2            => Kinematics.Configuration.RH_DH_R2,
         α_2            => Kinematics.Configuration.RH_DH_Alpha2,
         d_3            => Kinematics.Configuration.RH_DH_D3,
         r_3            => Kinematics.Configuration.RH_DH_R3,
         α_3            => Kinematics.Configuration.RH_DH_Alpha3,
         Posture        => Self.Scene.Legs_Posture (Legs.Right_Hind));

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

      Self.Viewport_Matrix    :=
        [[1.0, 0.0, 0.0, 0.0],
         [0.0, 1.0, 0.0, 0.0],
         [0.0, 0.0, 1.0, 0.0],
         [0.0, 0.0, 0.0, 1.0]];

      Self.On_Realize (Call => Dispatch_Realize'Access);
      Self.On_Resize (Call => Dispatch_Resize'Access);
      Self.On_Render (Call => Dispatch_Render'Access);
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
      VAO : aliased epoxy.GLuint;

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
