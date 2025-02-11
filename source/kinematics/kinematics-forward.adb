--
--  Copyright (C) 2023-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Implementation of this package use mathematical formalas derived in
--  inverse_kinematics-numerical.wxmx file. These formulas copy-pasted to the
--  files in the 'templates' subdirectory and preprocessed into the files in
--  'generated' subdirectory. All subprograms are organized as subunits, thus
--  mostly generated code is separated from the hand written code.
--
--  Formulas are used "as-is" without any preliminary optimization, so, it is
--  compiler responsibility to optimize their execution.

pragma Restrictions (No_Elaboration_Code);

with Kinematics.Configuration.Derived;

package body Kinematics.Forward is

   use type Reals.Real;

   procedure Compute_H_be_Matrix
     (Cos_Alpha_0 : Reals.Real;
      Sin_Alpha_0 : Reals.Real;
      Cos_Alpha_1 : Reals.Real;
      Sin_Alpha_1 : Reals.Real;
      Cos_Alpha_2 : Reals.Real;
      Sin_Alpha_2 : Reals.Real;
      Cos_Alpha_3 : Reals.Real;
      Sin_Alpha_3 : Reals.Real;
      Cos_Theta_1 : Reals.Real;
      Sin_Theta_1 : Reals.Real;
      Cos_Theta_2 : Reals.Real;
      Sin_Theta_2 : Reals.Real;
      Cos_Theta_3 : Reals.Real;
      Sin_Theta_3 : Reals.Real;
      R_1         : Reals.Real;
      R_2         : Reals.Real;
      R_3         : Reals.Real;
      B_x         : Reals.Real;
      B_y         : Reals.Real;
      B_z         : Reals.Real;
      M_11        : out Reals.Real;
      M_12        : out Reals.Real;
      M_13        : out Reals.Real;
      M_14        : out Reals.Real;
      M_21        : out Reals.Real;
      M_22        : out Reals.Real;
      M_23        : out Reals.Real;
      M_24        : out Reals.Real;
      M_31        : out Reals.Real;
      M_32        : out Reals.Real;
      M_33        : out Reals.Real;
      M_34        : out Reals.Real) is separate;

   procedure Compute_E_Position
     (B_x         : Reals.Real;
      B_y         : Reals.Real;
      B_z         : Reals.Real;
      Cos_Alpha_0 : Reals.Real;
      Sin_Alpha_0 : Reals.Real;
      R_1         : Reals.Real;
      Cos_Alpha_1 : Reals.Real;
      Sin_Alpha_1 : Reals.Real;
      R_2         : Reals.Real;
      Cos_Alpha_2 : Reals.Real;
      Sin_Alpha_2 : Reals.Real;
      R_3         : Reals.Real;
      Cos_Theta_1 : Reals.Real;
      Sin_Theta_1 : Reals.Real;
      Cos_Theta_2 : Reals.Real;
      Sin_Theta_2 : Reals.Real;
      Cos_Theta_3 : Reals.Real;
      Sin_Theta_3 : Reals.Real;
      X        : out Reals.Real;
      Y        : out Reals.Real;
      Z        : out Reals.Real) is separate;

   -------------------
   -- LF_E_Position --
   -------------------

   function LF_E_Position
     (Posture : Kinematics.Posture) return Reals.Vectors_3D.Vector_3D
   is
      Cos_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_1);
      Sin_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_1);
      Cos_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_2);
      Sin_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_2);
      Cos_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_3);
      Sin_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_3);

      X : Reals.Real;
      Y : Reals.Real;
      Z : Reals.Real;

   begin
      Compute_E_Position
        (B_x         => Kinematics.Configuration.LF_Base_X,
         B_y         => Kinematics.Configuration.LF_Base_Y,
         B_z         => Kinematics.Configuration.LF_Base_Z,
         Cos_Alpha_0 => Kinematics.Configuration.Derived.LF_Cos_Gamma_0,
         Sin_Alpha_0 => Kinematics.Configuration.Derived.LF_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.LF_DH_R1,
         Cos_Alpha_1 => Kinematics.Configuration.Derived.LF_Cos_Alpha_1,
         Sin_Alpha_1 => Kinematics.Configuration.Derived.LF_Sin_Alpha_1,
         R_2         => Kinematics.Configuration.LF_DH_R2,
         Cos_Alpha_2 => Kinematics.Configuration.Derived.LF_Cos_Alpha_2,
         Sin_Alpha_2 => Kinematics.Configuration.Derived.LF_Sin_Alpha_2,
         R_3         => Kinematics.Configuration.LF_DH_R3,
         Cos_Theta_1 => Cos_Theta_1,
         Sin_Theta_1 => Sin_Theta_1,
         Cos_Theta_2 => Cos_Theta_2,
         Sin_Theta_2 => Sin_Theta_2,
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         X           => X,
         Y           => Y,
         Z           => Z);

      return Reals.Vectors_3D.To_Vector_3D (X, Y, Z);
   end LF_E_Position;

   -------------------
   -- LF_E_Position --
   -------------------

   function LF_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position
   is
      Cos_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_1);
      Sin_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_1);
      Cos_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_2);
      Sin_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_2);
      Cos_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_3);
      Sin_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_3);

      X : Reals.Real;
      Y : Reals.Real;
      Z : Reals.Real;

   begin
      Compute_E_Position
        (B_x         => Kinematics.Configuration.LF_Base_X,
         B_y         => Kinematics.Configuration.LF_Base_Y,
         B_z         => Kinematics.Configuration.LF_Base_Z,
         Cos_Alpha_0 => Kinematics.Configuration.Derived.LF_Cos_Gamma_0,
         Sin_Alpha_0 => Kinematics.Configuration.Derived.LF_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.LF_DH_R1,
         Cos_Alpha_1 => Kinematics.Configuration.Derived.LF_Cos_Alpha_1,
         Sin_Alpha_1 => Kinematics.Configuration.Derived.LF_Sin_Alpha_1,
         R_2         => Kinematics.Configuration.LF_DH_R2,
         Cos_Alpha_2 => Kinematics.Configuration.Derived.LF_Cos_Alpha_2,
         Sin_Alpha_2 => Kinematics.Configuration.Derived.LF_Sin_Alpha_2,
         R_3         => Kinematics.Configuration.LF_DH_R3,
         Cos_Theta_1 => Cos_Theta_1,
         Sin_Theta_1 => Sin_Theta_1,
         Cos_Theta_2 => Cos_Theta_2,
         Sin_Theta_2 => Sin_Theta_2,
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         X           => X,
         Y           => Y,
         Z           => Z);

      return CGK.Primitives.Points_3D.As_Point_3D (X, Y, Z);
   end LF_E_Position;

   -------------
   -- LF_T_BE --
   -------------

   procedure LF_T_BE
     (Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real;
      Result  : out Reals.Transformations_3D.Transformation_3D)
   is
      Cos_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Theta_1);
      Sin_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Theta_1);
      Cos_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Theta_2);
      Sin_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Theta_2);
      Cos_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Theta_3);
      Sin_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Theta_3);

      M_11 : Reals.Real;
      M_12 : Reals.Real;
      M_13 : Reals.Real;
      M_14 : Reals.Real;
      M_21 : Reals.Real;
      M_22 : Reals.Real;
      M_23 : Reals.Real;
      M_24 : Reals.Real;
      M_31 : Reals.Real;
      M_32 : Reals.Real;
      M_33 : Reals.Real;
      M_34 : Reals.Real;

   begin
      Compute_H_be_Matrix
        (Cos_Alpha_0 => Kinematics.Configuration.Derived.LF_Cos_Gamma_0,
         Sin_Alpha_0 => Kinematics.Configuration.Derived.LF_Sin_Gamma_0,
         Cos_Alpha_1 => Kinematics.Configuration.Derived.LF_Cos_Alpha_1,
         Sin_Alpha_1 => Kinematics.Configuration.Derived.LF_Sin_Alpha_1,
         Cos_Alpha_2 => Kinematics.Configuration.Derived.LF_Cos_Alpha_2,
         Sin_Alpha_2 => Kinematics.Configuration.Derived.LF_Sin_Alpha_2,
         Cos_Alpha_3 => Kinematics.Configuration.Derived.LF_Cos_Alpha_3,
         Sin_Alpha_3 => Kinematics.Configuration.Derived.LF_Sin_Alpha_3,
         Cos_Theta_1 => Cos_Theta_1,
         Sin_Theta_1 => Sin_Theta_1,
         Cos_Theta_2 => Cos_Theta_2,
         Sin_Theta_2 => Sin_Theta_2,
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         R_1         => Kinematics.Configuration.LF_DH_R1,
         R_2         => Kinematics.Configuration.LF_DH_R2,
         R_3         => Kinematics.Configuration.LF_DH_R3,
         B_x         => Kinematics.Configuration.LF_Base_X,
         B_y         => Kinematics.Configuration.LF_Base_Y,
         B_z         => Kinematics.Configuration.LF_Base_Z,
         M_11        => M_11,
         M_12        => M_12,
         M_13        => M_13,
         M_14        => M_14,
         M_21        => M_21,
         M_22        => M_22,
         M_23        => M_23,
         M_24        => M_24,
         M_31        => M_31,
         M_32        => M_32,
         M_33        => M_33,
         M_34        => M_34);
      Reals.Transformations_3D.Initialize
        (Self => Result,
         M_11 => M_11,
         M_12 => M_12,
         M_13 => M_13,
         M_14 => M_14,
         M_21 => M_21,
         M_22 => M_22,
         M_23 => M_23,
         M_24 => M_24,
         M_31 => M_31,
         M_32 => M_32,
         M_33 => M_33,
         M_34 => M_34);
   end LF_T_BE;

   -------------------
   -- LH_E_Position --
   -------------------

   function LH_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position
   is
      Cos_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_1);
      Sin_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_1);
      Cos_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_2);
      Sin_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_2);
      Cos_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_3);
      Sin_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_3);

      X : Reals.Real;
      Y : Reals.Real;
      Z : Reals.Real;

   begin
      Compute_E_Position
        (B_x         => Kinematics.Configuration.LH_Base_X,
         B_y         => Kinematics.Configuration.LH_Base_Y,
         B_z         => Kinematics.Configuration.LH_Base_Z,
         Cos_Alpha_0 => Kinematics.Configuration.Derived.LH_Cos_Gamma_0,
         Sin_Alpha_0 => Kinematics.Configuration.Derived.LH_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.LH_DH_R1,
         Cos_Alpha_1 => Kinematics.Configuration.Derived.LH_Cos_Alpha_1,
         Sin_Alpha_1 => Kinematics.Configuration.Derived.LH_Sin_Alpha_1,
         R_2         => Kinematics.Configuration.LH_DH_R2,
         Cos_Alpha_2 => Kinematics.Configuration.Derived.LH_Cos_Alpha_2,
         Sin_Alpha_2 => Kinematics.Configuration.Derived.LH_Sin_Alpha_2,
         R_3         => Kinematics.Configuration.LH_DH_R3,
         Cos_Theta_1 => Cos_Theta_1,
         Sin_Theta_1 => Sin_Theta_1,
         Cos_Theta_2 => Cos_Theta_2,
         Sin_Theta_2 => Sin_Theta_2,
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         X           => X,
         Y           => Y,
         Z           => Z);

      return CGK.Primitives.Points_3D.As_Point_3D (X, Y, Z);
   end LH_E_Position;

   -------------------
   -- LM_E_Position --
   -------------------

   function LM_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position
   is
      Cos_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_1);
      Sin_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_1);
      Cos_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_2);
      Sin_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_2);
      Cos_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_3);
      Sin_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_3);

      X : Reals.Real;
      Y : Reals.Real;
      Z : Reals.Real;

   begin
      Compute_E_Position
        (B_x         => Kinematics.Configuration.LM_Base_X,
         B_y         => Kinematics.Configuration.LM_Base_Y,
         B_z         => Kinematics.Configuration.LM_Base_Z,
         Cos_Alpha_0 => Kinematics.Configuration.Derived.LM_Cos_Gamma_0,
         Sin_Alpha_0 => Kinematics.Configuration.Derived.LM_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.LM_DH_R1,
         Cos_Alpha_1 => Kinematics.Configuration.Derived.LM_Cos_Alpha_1,
         Sin_Alpha_1 => Kinematics.Configuration.Derived.LM_Sin_Alpha_1,
         R_2         => Kinematics.Configuration.LM_DH_R2,
         Cos_Alpha_2 => Kinematics.Configuration.Derived.LM_Cos_Alpha_2,
         Sin_Alpha_2 => Kinematics.Configuration.Derived.LM_Sin_Alpha_2,
         R_3         => Kinematics.Configuration.LM_DH_R3,
         Cos_Theta_1 => Cos_Theta_1,
         Sin_Theta_1 => Sin_Theta_1,
         Cos_Theta_2 => Cos_Theta_2,
         Sin_Theta_2 => Sin_Theta_2,
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         X           => X,
         Y           => Y,
         Z           => Z);

      return CGK.Primitives.Points_3D.As_Point_3D (X, Y, Z);
   end LM_E_Position;

   -------------------
   -- RF_E_Position --
   -------------------

   function RF_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position
   is
      Cos_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_1);
      Sin_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_1);
      Cos_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_2);
      Sin_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_2);
      Cos_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_3);
      Sin_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_3);

      X : Reals.Real;
      Y : Reals.Real;
      Z : Reals.Real;

   begin
      Compute_E_Position
        (B_x         => Kinematics.Configuration.RF_Base_X,
         B_y         => Kinematics.Configuration.RF_Base_Y,
         B_z         => Kinematics.Configuration.RF_Base_Z,
         Cos_Alpha_0 => Kinematics.Configuration.Derived.RF_Cos_Gamma_0,
         Sin_Alpha_0 => Kinematics.Configuration.Derived.RF_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.RF_DH_R1,
         Cos_Alpha_1 => Kinematics.Configuration.Derived.RF_Cos_Alpha_1,
         Sin_Alpha_1 => Kinematics.Configuration.Derived.RF_Sin_Alpha_1,
         R_2         => Kinematics.Configuration.RF_DH_R2,
         Cos_Alpha_2 => Kinematics.Configuration.Derived.RF_Cos_Alpha_2,
         Sin_Alpha_2 => Kinematics.Configuration.Derived.RF_Sin_Alpha_2,
         R_3         => Kinematics.Configuration.RF_DH_R3,
         Cos_Theta_1 => Cos_Theta_1,
         Sin_Theta_1 => Sin_Theta_1,
         Cos_Theta_2 => Cos_Theta_2,
         Sin_Theta_2 => Sin_Theta_2,
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         X           => X,
         Y           => Y,
         Z           => Z);

      return CGK.Primitives.Points_3D.As_Point_3D (X, Y, Z);
   end RF_E_Position;

   -------------------
   -- RH_E_Position --
   -------------------

   function RH_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position
   is
      Cos_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_1);
      Sin_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_1);
      Cos_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_2);
      Sin_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_2);
      Cos_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_3);
      Sin_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_3);

      X : Reals.Real;
      Y : Reals.Real;
      Z : Reals.Real;

   begin
      Compute_E_Position
        (B_x         => Kinematics.Configuration.RH_Base_X,
         B_y         => Kinematics.Configuration.RH_Base_Y,
         B_z         => Kinematics.Configuration.RH_Base_Z,
         Cos_Alpha_0 => Kinematics.Configuration.Derived.RH_Cos_Gamma_0,
         Sin_Alpha_0 => Kinematics.Configuration.Derived.RH_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.RH_DH_R1,
         Cos_Alpha_1 => Kinematics.Configuration.Derived.RH_Cos_Alpha_1,
         Sin_Alpha_1 => Kinematics.Configuration.Derived.RH_Sin_Alpha_1,
         R_2         => Kinematics.Configuration.RH_DH_R2,
         Cos_Alpha_2 => Kinematics.Configuration.Derived.RH_Cos_Alpha_2,
         Sin_Alpha_2 => Kinematics.Configuration.Derived.RH_Sin_Alpha_2,
         R_3         => Kinematics.Configuration.RH_DH_R3,
         Cos_Theta_1 => Cos_Theta_1,
         Sin_Theta_1 => Sin_Theta_1,
         Cos_Theta_2 => Cos_Theta_2,
         Sin_Theta_2 => Sin_Theta_2,
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         X           => X,
         Y           => Y,
         Z           => Z);

      return CGK.Primitives.Points_3D.As_Point_3D (X, Y, Z);
   end RH_E_Position;

   -------------------
   -- RM_E_Position --
   -------------------

   function RM_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position
   is
      Cos_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_1);
      Sin_Theta_1 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_1);
      Cos_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_2);
      Sin_Theta_2 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_2);
      Cos_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Cos (Posture.Theta.M_3);
      Sin_Theta_3 : constant Reals.Real :=
        Reals.Elementary_Functions.Sin (Posture.Theta.M_3);

      X : Reals.Real;
      Y : Reals.Real;
      Z : Reals.Real;

   begin
      Compute_E_Position
        (B_x         => Kinematics.Configuration.RM_Base_X,
         B_y         => Kinematics.Configuration.RM_Base_Y,
         B_z         => Kinematics.Configuration.RM_Base_Z,
         Cos_Alpha_0 => Kinematics.Configuration.Derived.RM_Cos_Gamma_0,
         Sin_Alpha_0 => Kinematics.Configuration.Derived.RM_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.RM_DH_R1,
         Cos_Alpha_1 => Kinematics.Configuration.Derived.RM_Cos_Alpha_1,
         Sin_Alpha_1 => Kinematics.Configuration.Derived.RM_Sin_Alpha_1,
         R_2         => Kinematics.Configuration.RM_DH_R2,
         Cos_Alpha_2 => Kinematics.Configuration.Derived.RM_Cos_Alpha_2,
         Sin_Alpha_2 => Kinematics.Configuration.Derived.RM_Sin_Alpha_2,
         R_3         => Kinematics.Configuration.RM_DH_R3,
         Cos_Theta_1 => Cos_Theta_1,
         Sin_Theta_1 => Sin_Theta_1,
         Cos_Theta_2 => Cos_Theta_2,
         Sin_Theta_2 => Sin_Theta_2,
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         X           => X,
         Y           => Y,
         Z           => Z);

      return CGK.Primitives.Points_3D.As_Point_3D (X, Y, Z);
   end RM_E_Position;

end Kinematics.Forward;
