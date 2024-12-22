--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Gtk.Grid;
private with Gtk.Scale;
with Gtk.Widget;

package GUI.Controls_Panels is

   type Controls_Panel_Record is new Gtk.Widget.Gtk_Widget_Record with private;

   type Controls_Panel is access all Controls_Panel_Record'Class;

   procedure Gtk_New (Self : out Controls_Panel);

   --  procedure Set_Horizontal_Rotation
   --    (Self  : in out Graphics_View_Record'Class;
   --     Angle : Glib.Gdouble);
   --
   --  procedure Set_Vertical_Rotation
   --    (Self  : in out Graphics_View_Record'Class;
   --     Angle : Glib.Gdouble);
   --
   --  procedure Set_Scale
   --    (Self : in out Graphics_View_Record'Class;
   --     To   : Glib.Gdouble);

private

   --  type Line_Program_Access is access all GUI.Programs.Lines.Line_Program;

   type Controls_Panel_Record is new Gtk.Grid.Gtk_Grid_Record with record
      Velocity_X : Gtk.Scale.Gtk_Scale;
      Velocity_Y : Gtk.Scale.Gtk_Scale;
      Velocity_W : Gtk.Scale.Gtk_Scale;
   end record;

end GUI.Controls_Panels;
