--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Glib;
private with Gtk.GLArea;
with Gtk.Widget;
private with OpenGL;

private with Telemetry.GUI.Programs.Lines;

package Telemetry.GUI.Graphics_Views is

   type Graphics_View_Record is new Gtk.Widget.Gtk_Widget_Record with private;

   type Graphics_View is access all Graphics_View_Record'Class;

   procedure Gtk_New (Self : out Graphics_View);

   procedure Set_Horizontal_Rotation
     (Self  : in out Graphics_View_Record'Class;
      Angle : Glib.Gdouble);

   procedure Set_Vertical_Rotation
     (Self  : in out Graphics_View_Record'Class;
      Angle : Glib.Gdouble);

private

   type Line_Program_Access is
     access all Telemetry.GUI.Programs.Lines.Line_Program;

   type Graphics_View_Record is new Gtk.GLArea.Gtk_GLArea_Record with record
      Horizontal_Angle : OpenGL.GLfloat;
      Vertical_Angle   : OpenGL.GLfloat;
      Viewport_Matrix  : OpenGL.GLfloat_Matrix_4x4;

      Line_Program     : Line_Program_Access;
   end record;

end Telemetry.GUI.Graphics_Views;
