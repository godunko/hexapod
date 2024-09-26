--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with CGK.Reals;

package body Legs.Workspace is

   use type CGK.Reals.Real;

   type Workspace is record
      Center_X : Reals.Real;
      Center_Y : Reals.Real;
      Center_Z : Reals.Real;
      Radius   : Reals.Real;
   end record;
   --  Leg's workspace at ground level.

   Workspaces : array (Leg_Index) of Workspace;

   procedure Compute_Workspace
     (Leg         : Leg_Information;
      Self        : in out Workspace;
      Body_Height : Reals.Real);

   -------------
   -- Compute --
   -------------

   procedure Compute (Body_Height : Reals.Real) is
   begin
      for J in Leg_Index loop
         Compute_Workspace (Legs (J), Workspaces (J), Body_Height);
      end loop;
   end Compute;

   -----------------------
   -- Compute_Workspace --
   -----------------------

   procedure Compute_Workspace
     (Leg         : Leg_Information;
      Self        : in out Workspace;
      Body_Height : Reals.Real)
   is
      L        : constant Reals.Real := Leg.R_2 + Leg.R_3;
      H        : constant Reals.Real := Leg.Z_0 + Body_Height;
      Diameter : constant Reals.Real :=
        Reals.Elementary_Functions.Sqrt (L * L - H * H);
      Radius   : constant Reals.Real := Diameter / 2.0;
      Offset   : constant Reals.Real := Radius + Leg.R_1;

   begin
      Self :=
        (Center_X => Offset * Leg.Cos_Gamma_0 + Leg.X_0,
         Center_Y => Offset * Leg.Sin_Gamma_0 + Leg.Y_0,
         Center_Z => -Body_Height,
         Radius   => Radius);
   end Compute_Workspace;

   ------------------------
   -- Get_Bounded_Circle --
   ------------------------

   function Get_Bounded_Circle
     (Leg : Leg_Index) return CGK.Primitives.Circles_2D.Circle_2D is
   begin
      return CGK.Primitives.Circles_2D.Create_Circle_2D
        (X      => Workspaces (Leg).Center_X,
         Y      => Workspaces (Leg).Center_Y,
         Radius => Workspaces (Leg).Radius * 0.4);
   end Get_Bounded_Circle;

   ------------------------
   -- Get_Bounding_Shape --
   ------------------------

   --  procedure Get_Bounding_Shape
   --    (Index  : Leg_Index;
   --     Result : out CGK.Primitives.Circles_2D.Circle_2D) is
   --  begin
   --     Result :=
   --       CGK.Primitives.Circles_2D.Create_Circle_2D
   --         (X      => CGK.Reals.Real (Workspaces (Index).Center_X),
   --          Y      => CGK.Reals.Real (Workspaces (Index).Center_Y),
   --          Radius => CGK.Reals.Real (Workspaces (Index).Radius));
   --  end Get_Bounding_Shape;

   ------------------------
   -- Get_Bounding_Shape --
   ------------------------

   --  function Get_Bounding_Shape
   --    (Index  : Leg_Index) return CGK.Primitives.Circles_2D.Circle_2D is
   --  begin
   --     return
   --       CGK.Primitives.Circles_2D.Create_Circle_2D
   --         (X      => CGK.Reals.Real (Workspaces (Index).Center_X),
   --          Y      => CGK.Reals.Real (Workspaces (Index).Center_Y),
   --          Radius => CGK.Reals.Real (Workspaces (Index).Radius));
   --  end Get_Bounding_Shape;

   -------------------
   -- Ground_Center --
   -------------------

   procedure Ground_Center
     (Index    : Leg_Index;
      Position : out Kinematics.Position) is
   begin
      Kinematics.Set
        (Self => Position,
         X    => Workspaces (Index).Center_X,
         Y    => Workspaces (Index).Center_Y,
         Z    => Workspaces (Index).Center_Z);
   end Ground_Center;

end Legs.Workspace;
