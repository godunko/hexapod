--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

private with Gtk.GLArea;
with Gtk.Widget;

package Telemetry.GUI.Graphics_Views is

   type Graphics_View_Record is new Gtk.Widget.Gtk_Widget_Record with private;

   type Graphics_View is access all Graphics_View_Record'Class;

   procedure Gtk_New (Self : out Graphics_View);
   --  procedure Initialize (Self : not null access Graphic_View_Record'Class);

private

   type Graphics_View_Record is new Gtk.GLArea.Gtk_GLArea_Record with record
      null;
   end record;

end Telemetry.GUI.Graphics_Views;
