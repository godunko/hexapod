--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  pragma Restrictions (No_Elaboration_Code);

with CGK.Primitives.XYZs;

with Kinematics.Configuration;
with Kinematics.Inverse.Geometric;

package body Legs is

   type Leg_Base_Parameters is record
      X     : Reals.Real;
      Y     : Reals.Real;
      Z     : Reals.Real;
      Gamma : Reals.Real;
   end record;

   type Leg_Segment_Parameters is record
      Alpha : Reals.Real;
      D     : Reals.Real;
      R     : Reals.Real;
   end record;

   procedure Initialize
     (Self      : out Leg_Kinematics_Parameters;
      Side      : Leg_Side;
      Base      : Leg_Base_Parameters;
      Segment_1 : Leg_Segment_Parameters;
      Segment_2 : Leg_Segment_Parameters;
      Segment_3 : Leg_Segment_Parameters);

   ------------------------
   -- Forward_Kinematics --
   ------------------------

   procedure Forward_Kinematics
     (Self     : Leg_Kinematics_Parameters;
      Posture  : Kinematics.Posture;
      Base     : out CGK.Primitives.Points_3D.Point_3D;
      Joint_1  : out CGK.Primitives.Points_3D.Point_3D;
      Joint_2  : out CGK.Primitives.Points_3D.Point_3D;
      Joint_3  : out CGK.Primitives.Points_3D.Point_3D;
      Effector : out CGK.Primitives.Points_3D.Point_3D)
   is
      Origin : constant CGK.Primitives.Points_3D.Point_3D :=
        CGK.Primitives.Points_3D.As_Point_3D (0.0, 0.0, 0.0);

      T      : CGK.Primitives.Transformations_3D.Transformation_3D;
      T_1_2  : CGK.Primitives.Transformations_3D.Transformation_3D;
      T_2_3  : CGK.Primitives.Transformations_3D.Transformation_3D;
      T_3_E  : CGK.Primitives.Transformations_3D.Transformation_3D;

   begin
      Base     := Origin;
      Joint_1  := Origin;
      Joint_2  := Origin;
      Joint_3  := Origin;
      Effector := Origin;

      CGK.Primitives.Transformations_3D.Set_Denavit_Hartenberg
        (Self => T_1_2,
         d    => Self.D_1,
         θ    => Kinematics.Theta_1 (Posture),
         r    => Self.R_1,
         α    => Self.α_1);
      CGK.Primitives.Transformations_3D.Set_Denavit_Hartenberg
        (Self => T_2_3,
         d    => Self.D_2,
         θ    => Kinematics.Theta_2 (Posture),
         r    => Self.R_2,
         α    => Self.α_2);
      CGK.Primitives.Transformations_3D.Set_Denavit_Hartenberg
        (Self => T_3_E,
         d    => Self.D_3,
         θ    => Kinematics.Theta_3 (Posture),
         r    => Self.R_3,
         α    => Self.α_3);

      T := Self.T_B_1;
      CGK.Primitives.Points_3D.Transform (Joint_1, T);

      CGK.Primitives.Transformations_3D.Multiply (T, T_1_2);
      CGK.Primitives.Points_3D.Transform (Joint_2, T);

      CGK.Primitives.Transformations_3D.Multiply (T, T_2_3);
      CGK.Primitives.Points_3D.Transform (Joint_3, T);

      CGK.Primitives.Transformations_3D.Multiply (T, T_3_E);
      CGK.Primitives.Points_3D.Transform (Effector, T);
   end Forward_Kinematics;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : out Leg_Kinematics_Parameters;
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

      Self.D_1     := Segment_1.D;
      Self.R_1     := Segment_1.R;
      Self.α_1     := Segment_1.Alpha;

      Self.D_2     := Segment_2.D;
      Self.R_2     := Segment_2.R;
      Self.α_2     := Segment_2.Alpha;

      Self.D_3     := Segment_3.D;
      Self.R_3     := Segment_3.R;
      Self.α_3     := Segment_3.Alpha;

      Self.Cos_Gamma_0 := Reals.Elementary_Functions.Cos (Self.Gamma_0);
      Self.Sin_Gamma_0 := Reals.Elementary_Functions.Sin (Self.Gamma_0);

      CGK.Primitives.Transformations_3D.Set_Identity (Self.T_B_1);
      CGK.Primitives.Transformations_3D.Translate
        (Self.T_B_1, CGK.Primitives.XYZs.As_XYZ (Base.X, Base.Y, Base.Z));
      CGK.Primitives.Transformations_3D.Rotate_Z (Self.T_B_1, Base.Gamma);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialize
        (Self      => Legs (Left_Front),
         Side      => Standard.Legs.Left,
         Base      =>
           (X     => Kinematics.Configuration.LF_Base_X,
            Y     => Kinematics.Configuration.LF_Base_Y,
            Z     => Kinematics.Configuration.LF_Base_Z,
            Gamma => Kinematics.Configuration.LF_Base_Gamma),
         Segment_1 =>
           (R     => Kinematics.Configuration.LF_DH_R1,
            D     => Kinematics.Configuration.LF_DH_D1,
            Alpha => Kinematics.Configuration.LF_DH_Alpha1),
         Segment_2 =>
           (R     => Kinematics.Configuration.LF_DH_R2,
            D     => Kinematics.Configuration.LF_DH_D2,
            Alpha => Kinematics.Configuration.LF_DH_Alpha2),
         Segment_3 =>
           (R     => Kinematics.Configuration.LF_DH_R3,
            D     => Kinematics.Configuration.LF_DH_D3,
            Alpha => Kinematics.Configuration.LF_DH_Alpha3));
      Initialize
        (Self      => Legs (Left_Middle),
         Side      => Standard.Legs.Left,
         Base      =>
           (X     => Kinematics.Configuration.LM_Base_X,
            Y     => Kinematics.Configuration.LM_Base_Y,
            Z     => Kinematics.Configuration.LM_Base_Z,
            Gamma => Kinematics.Configuration.LM_Base_Gamma),
         Segment_1 =>
           (R     => Kinematics.Configuration.LM_DH_R1,
            D     => Kinematics.Configuration.LM_DH_D1,
            Alpha => Kinematics.Configuration.LM_DH_Alpha1),
         Segment_2 =>
           (R     => Kinematics.Configuration.LM_DH_R2,
            D     => Kinematics.Configuration.LM_DH_D2,
            Alpha => Kinematics.Configuration.LM_DH_Alpha2),
         Segment_3 =>
           (R     => Kinematics.Configuration.LM_DH_R3,
            D     => Kinematics.Configuration.LM_DH_D3,
            Alpha => Kinematics.Configuration.LM_DH_Alpha3));
      Initialize
        (Self      => Legs (Left_Hind),
         Side      => Standard.Legs.Left,
         Base      =>
           (X     => Kinematics.Configuration.LH_Base_X,
            Y     => Kinematics.Configuration.LH_Base_Y,
            Z     => Kinematics.Configuration.LH_Base_Z,
            Gamma => Kinematics.Configuration.LH_Base_Gamma),
         Segment_1 =>
           (R     => Kinematics.Configuration.LH_DH_R1,
            D     => Kinematics.Configuration.LH_DH_D1,
            Alpha => Kinematics.Configuration.LH_DH_Alpha1),
         Segment_2 =>
           (R     => Kinematics.Configuration.LH_DH_R2,
            D     => Kinematics.Configuration.LH_DH_D2,
            Alpha => Kinematics.Configuration.LH_DH_Alpha2),
         Segment_3 =>
           (R     => Kinematics.Configuration.LH_DH_R3,
            D     => Kinematics.Configuration.LH_DH_D3,
            Alpha => Kinematics.Configuration.LH_DH_Alpha3));
      Initialize
        (Self      => Legs (Right_Front),
         Side      => Standard.Legs.Right,
         Base      =>
           (X     => Kinematics.Configuration.RF_Base_X,
            Y     => Kinematics.Configuration.RF_Base_Y,
            Z     => Kinematics.Configuration.RF_Base_Z,
            Gamma => Kinematics.Configuration.RF_Base_Gamma),
         Segment_1 =>
           (R     => Kinematics.Configuration.RF_DH_R1,
            D     => Kinematics.Configuration.RF_DH_D1,
            Alpha => Kinematics.Configuration.RF_DH_Alpha1),
         Segment_2 =>
           (R     => Kinematics.Configuration.RF_DH_R2,
            D     => Kinematics.Configuration.RF_DH_D2,
            Alpha => Kinematics.Configuration.RF_DH_Alpha2),
         Segment_3 =>
           (R     => Kinematics.Configuration.RF_DH_R3,
            D     => Kinematics.Configuration.RF_DH_D3,
            Alpha => Kinematics.Configuration.RF_DH_Alpha3));
      Initialize
        (Self      => Legs (Right_Middle),
         Side      => Standard.Legs.Right,
         Base      =>
           (X     => Kinematics.Configuration.RM_Base_X,
            Y     => Kinematics.Configuration.RM_Base_Y,
            Z     => Kinematics.Configuration.RM_Base_Z,
            Gamma => Kinematics.Configuration.RM_Base_Gamma),
         Segment_1 =>
           (R     => Kinematics.Configuration.RM_DH_R1,
            D     => Kinematics.Configuration.RM_DH_D1,
            Alpha => Kinematics.Configuration.RM_DH_Alpha1),
         Segment_2 =>
           (R     => Kinematics.Configuration.RM_DH_R2,
            D     => Kinematics.Configuration.RM_DH_D2,
            Alpha => Kinematics.Configuration.RM_DH_Alpha2),
         Segment_3 =>
           (R     => Kinematics.Configuration.RM_DH_R3,
            D     => Kinematics.Configuration.RM_DH_D3,
            Alpha => Kinematics.Configuration.RM_DH_Alpha3));
      Initialize
        (Self      => Legs (Right_Hind),
         Side      => Standard.Legs.Right,
         Base      =>
           (X     => Kinematics.Configuration.RH_Base_X,
            Y     => Kinematics.Configuration.RH_Base_Y,
            Z     => Kinematics.Configuration.RH_Base_Z,
            Gamma => Kinematics.Configuration.RH_Base_Gamma),
         Segment_1 =>
           (R     => Kinematics.Configuration.RH_DH_R1,
            D     => Kinematics.Configuration.RH_DH_D1,
            Alpha => Kinematics.Configuration.RH_DH_Alpha1),
         Segment_2 =>
           (R     => Kinematics.Configuration.RH_DH_R2,
            D     => Kinematics.Configuration.RH_DH_D2,
            Alpha => Kinematics.Configuration.RH_DH_Alpha2),
         Segment_3 =>
           (R     => Kinematics.Configuration.RH_DH_R3,
            D     => Kinematics.Configuration.RH_DH_D3,
            Alpha => Kinematics.Configuration.RH_DH_Alpha3));
   end Initialize;

   ------------------------
   -- Inverse_Kinematics --
   ------------------------

   procedure Inverse_Kinematics
     (Self             : Leg_Kinematics_Parameters;
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

end Legs;
