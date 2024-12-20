--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Command_Line;

with Glib.Application;
with Gtk.Application;
with Gtk.Application_Window;

with Telemetry.GUI.Graphics_Views;

package body Telemetry.GUI.Main_Window is

   App : Gtk.Application.Gtk_Application;
   AW  : Gtk.Application_Window.Gtk_Application_Window;

   GV  : Telemetry.GUI.Graphics_Views.Graphics_View;

   procedure On_Activate
     (Self : access Glib.Application.Gapplication_Record'Class);

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

      Telemetry.GUI.Graphics_Views.Gtk_New (GV);
      AW.Add (GV);

      AW.Show_All;
   end On_Activate;

end Telemetry.GUI.Main_Window;
