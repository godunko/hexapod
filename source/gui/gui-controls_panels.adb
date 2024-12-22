--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Glib;
with Gtk.Adjustment;

with CGK.Reals;

with Legs.Gait_Generator;

package body GUI.Controls_Panels is

   procedure Initialize (Self : in out Controls_Panel_Record'Class);

   Velocity_X_Adjustment : Gtk.Adjustment.Gtk_Adjustment;
   Velocity_Y_Adjustment : Gtk.Adjustment.Gtk_Adjustment;
   Velocity_W_Adjustment : Gtk.Adjustment.Gtk_Adjustment;

   procedure Dispatch_Velocity_Changed
     (Self : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   -------------------------------
   -- Dispatch_Velocity_Changed --
   -------------------------------

   procedure Dispatch_Velocity_Changed
     (Self : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      pragma Unreferenced (Self);

   begin
      Legs.Gait_Generator.Set_Velocity
        (RVX => CGK.Reals.Real (Velocity_X_Adjustment.Get_Value),
         RVY => CGK.Reals.Real (Velocity_Y_Adjustment.Get_Value),
         RVW => CGK.Reals.Real (Velocity_W_Adjustment.Get_Value));
   end Dispatch_Velocity_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Controls_Panel) is
   begin
      Self := new Controls_Panel_Record;
      Gtk.Grid.Initialize (Self);

      Initialize (Self.all);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Controls_Panel_Record'Class) is
      use type Glib.Gdouble;

   begin
      Velocity_X_Adjustment :=
        Gtk.Adjustment.Gtk_Adjustment_New
          (Value          => 0.0,
           Lower          => -1.0,
           Upper          => 1.0,
           Step_Increment => 0.01,
           Page_Increment => 0.1);
      Velocity_Y_Adjustment :=
        Gtk.Adjustment.Gtk_Adjustment_New
          (Value          => 0.0,
           Lower          => -1.0,
           Upper          => 1.0,
           Step_Increment => 0.01,
           Page_Increment => 0.1);
      Velocity_W_Adjustment :=
        Gtk.Adjustment.Gtk_Adjustment_New
          (Value          => 0.0,
           Lower          => -1.0,
           Upper          => 1.0,
           Step_Increment => 0.01,
           Page_Increment => 0.1);

      Velocity_X_Adjustment.On_Value_Changed
        (Dispatch_Velocity_Changed'Access);
      Velocity_Y_Adjustment.On_Value_Changed
        (Dispatch_Velocity_Changed'Access);
      Velocity_W_Adjustment.On_Value_Changed
        (Dispatch_Velocity_Changed'Access);

      Self.Velocity_X :=
        Gtk.Scale.Gtk_Vscale_New (Velocity_X_Adjustment);
      Self.Velocity_X.Set_Has_Origin (False);
      Self.Velocity_X.Set_Inverted (True);
      Self.Velocity_X.Set_Vexpand (True);

      Self.Velocity_Y :=
        Gtk.Scale.Gtk_Vscale_New (Velocity_Y_Adjustment);
      Self.Velocity_Y.Set_Has_Origin (False);
      Self.Velocity_X.Set_Inverted (True);
      Self.Velocity_Y.Set_Vexpand (True);

      Self.Velocity_W :=
        Gtk.Scale.Gtk_Vscale_New (Velocity_W_Adjustment);
      Self.Velocity_W.Set_Has_Origin (False);
      Self.Velocity_W.Set_Vexpand (True);

      Self.Attach (Self.Velocity_X, 0, 0, 1, 1);
      Self.Attach (Self.Velocity_Y, 1, 0, 1, 1);
      Self.Attach (Self.Velocity_W, 2, 0, 1, 1);
   end Initialize;

end GUI.Controls_Panels;
