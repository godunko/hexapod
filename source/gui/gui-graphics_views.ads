--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

private with Ada.Real_Time;

with Glib;
private with Glib.Main;
private with Gtk.GLArea;
with Gtk.Widget;

private with OpenGL;

private with GUI.Programs.Lines;
private with GUI.Scene_States;

package GUI.Graphics_Views is

   type Graphics_View_Record is new Gtk.Widget.Gtk_Widget_Record with private;

   type Graphics_View is access all Graphics_View_Record'Class;

   procedure Gtk_New (Self : out Graphics_View);

   procedure Set_Horizontal_Rotation
     (Self  : in out Graphics_View_Record'Class;
      Angle : Glib.Gdouble);

   procedure Set_Vertical_Rotation
     (Self  : in out Graphics_View_Record'Class;
      Angle : Glib.Gdouble);

   procedure Set_Scale
     (Self : in out Graphics_View_Record'Class;
      To   : Glib.Gdouble);

private

   type Line_Program_Access is access all GUI.Programs.Lines.Line_Program;

   type Graphics_View_Record is new Gtk.GLArea.Gtk_GLArea_Record with record
      Horizontal_Angle   : OpenGL.GLfloat;
      Vertical_Angle     : OpenGL.GLfloat;
      Scale              : OpenGL.GLfloat;
      Viewport_Matrix    : OpenGL.GLfloat_Matrix_4x4;

      Scene              : GUI.Scene_States.Scene_Information;

      Line_Program       : Line_Program_Access;
      Line_Elements      : OpenGL.GLsizei;

      Tick_Duration      : Ada.Real_Time.Time_Span;
      Next_Tick          : Ada.Real_Time.Time;
      Frame_Counter      : Natural;
      Skip_Frame_Counter : Natural;

      Timer_Source       : Glib.Main.G_Source_Id;
   end record;

end GUI.Graphics_Views;
