--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with Kinematics.Inverse.Geometric;

package body Legs is

   -----------------------
   -- Compute_Workspace --
   -----------------------

   procedure Compute_Workspace
     (Self        : in out Leg;
      Body_Height : Reals.Real)
   is
      use type Reals.Real;

      L        : constant Reals.Real := Self.R_2 + Self.R_3;
      H        : constant Reals.Real := Self.Z_0 + Body_Height;
      Diameter : constant Reals.Real :=
        Reals.Elementary_Functions.Sqrt (L * L - H * H);
      Radius   : constant Reals.Real := Diameter / 2.0;
      Offset   : constant Reals.Real := Radius + Self.R_1;

   begin
      Self.Workspace :=
        (Center_X => Offset * Self.Cos_Gamma_0 + Self.X_0,
         Center_Y => Offset * Self.Sin_Gamma_0 + Self.Y_0,
         Center_Z => -Body_Height,
         Radius   => Radius);
   end Compute_Workspace;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : out Leg;
      Side      : Leg_Side;
      Base      : Leg_Base_Parameters;
      Segment_1 : Leg_Segment_Parameters;
      Segment_2 : Leg_Segment_Parameters;
      Segment_3 : Leg_Segment_Parameters) is
   begin
      Self.Side    := Side;

      Self.X_0     := Base.X;
      Self.Y_0     := Base.Y;
      Self.Z_0     := Base.Z;
      Self.Gamma_0 := Base.Gamma;

      Self.R_1     := Segment_1.R;

      Self.R_2     := Segment_2.R;

      Self.R_3     := Segment_3.R;

      Self.Cos_Gamma_0 := Reals.Elementary_Functions.Cos (Self.Gamma_0);
      Self.Sin_Gamma_0 := Reals.Elementary_Functions.Sin (Self.Gamma_0);
   end Initialize;

   ------------------------
   -- Inverse_Kinematics --
   ------------------------

   procedure Inverse_Kinematics
     (Self             : Leg;
      Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean) is
   begin
      Kinematics.Inverse.Geometric.Solve
        (B_X              => Self.X_0,
         B_Y              => Self.Y_0,
         B_Z              => Self.Z_0,
         Cos_Gamma_0      => Self.Cos_Gamma_0,
         Sin_Gamma_0      => Self.Sin_Gamma_0,
         R_1              => Self.R_1,
         R_2              => Self.R_2,
         R_3              => Self.R_3,
         Inverse          => Self.Side = Left,
         Desired_Position => Desired_Position,
         Found_Posture    => Found_Posture,
         Success          => Success);
   end Inverse_Kinematics;

   ----------------------
   -- Workspace_Center --
   ----------------------

   procedure Workspace_Center
     (Self     : Leg;
      Position : out Kinematics.Position) is
   begin
      Kinematics.Set
        (Self => Position,
         X    => Self.Workspace.Center_X,
         Y    => Self.Workspace.Center_Y,
         Z    => Self.Workspace.Center_Z);
   end Workspace_Center;

end Legs;
