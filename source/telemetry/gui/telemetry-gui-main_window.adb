--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Command_Line;

with Glib.Application;
with Glib.Object;
with Gtk.Adjustment;
with Gtk.Application;
with Gtk.Application_Window;
with Gtk.Enums;
with Gtk.Grid;
with Gtk.Scale;

with Telemetry.GUI.Graphics_Views;

package body Telemetry.GUI.Main_Window is

   App : Gtk.Application.Gtk_Application;
   AW  : Gtk.Application_Window.Gtk_Application_Window;

   GV  : Telemetry.GUI.Graphics_Views.Graphics_View;
   G   : Gtk.Grid.Gtk_Grid;
   VA  : Gtk.Adjustment.Gtk_Adjustment;
   VS  : Gtk.Scale.Gtk_Scale;
   HA  : Gtk.Adjustment.Gtk_Adjustment;
   HS  : Gtk.Scale.Gtk_Scale;

   procedure On_Activate
     (Self : access Glib.Application.Gapplication_Record'Class);

   procedure Dispatch_Horizontal_Value_Changed
     (Self : access Glib.Object.GObject_Record'Class);

   procedure Dispatch_Vertical_Value_Changed
     (Self : access Glib.Object.GObject_Record'Class);

   ---------------------------------------
   -- Dispatch_Horizontal_Value_Changed --
   ---------------------------------------

   procedure Dispatch_Horizontal_Value_Changed
     (Self : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self);

   begin
      GV.Set_Horizontal_Rotation (HA.Get_Value);
   end Dispatch_Horizontal_Value_Changed;

   -------------------------------------
   -- Dispatch_Vertical_Value_Changed --
   -------------------------------------

   procedure Dispatch_Vertical_Value_Changed
     (Self : access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self);

   begin
      GV.Set_Vertical_Rotation (VA.Get_Value);
   end Dispatch_Vertical_Value_Changed;

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
     (Self : access Glib.Application.Gapplication_Record'Class)
   is
      use type Glib.Gdouble;

   begin
      Telemetry.GUI.Graphics_Views.Gtk_New (GV);
      GV.Set_Hexpand (True);
      GV.Set_Vexpand (True);

      VA :=
        Gtk.Adjustment.Gtk_Adjustment_New
          (Value          => -70.0,
           Lower          => -90.0,
           Upper          => 90.0,
           Step_Increment => 1.0,
           Page_Increment => 10.0);
      VA.On_Value_Changed
        (Call => Dispatch_Vertical_Value_Changed'Access, Slot => Self);

      VS := Gtk.Scale.Gtk_Vscale_New (VA);
      VS.Set_Has_Origin (False);
      VS.Set_Value_Pos (Gtk.Enums.Pos_Bottom);

      HA :=
        Gtk.Adjustment.Gtk_Adjustment_New
          (Value          => 30.0,
           Lower          => -180.0,
           Upper          => 180.0,
           Step_Increment => 1.0,
           Page_Increment => 10.0);
      HA.On_Value_Changed
        (Call => Dispatch_Horizontal_Value_Changed'Access, Slot => Self);

      HS := Gtk.Scale.Gtk_Hscale_New (HA);
      HS.Set_Has_Origin (False);
      HS.Set_Value_Pos (Gtk.Enums.Pos_Right);

      Gtk.Grid.Gtk_New (G);
      G.Attach (GV, 0, 0, 1, 1);
      G.Attach (VS, 1, 0, 1, 1);
      G.Attach (HS, 0, 1, 1, 1);

      AW :=
        Gtk.Application_Window.Gtk_Application_Window_New
          (Gtk.Application.Gtk_Application (Self));
      AW.Add (G);
      AW.Show_All;

      GV.Set_Horizontal_Rotation (HA.Get_Value);
      GV.Set_Vertical_Rotation (VA.Get_Value);
   end On_Activate;

end Telemetry.GUI.Main_Window;
