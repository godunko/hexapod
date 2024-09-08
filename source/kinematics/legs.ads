--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with Kinematics;
with Reals;

package Legs
  with Pure
is

   type Workspace is record
      null;
   end record;
   --  Leg's workspace at ground level.

   type Leg_Side is (Left, Right);

   type Leg is record
      Side        : Leg_Side;

      X_0         : Reals.Real;
      Y_0         : Reals.Real;
      Z_0         : Reals.Real;
      Gamma_0     : Reals.Real;

      Cos_Gamma_0 : Reals.Real;
      Sin_Gamma_0 : Reals.Real;

      R_1         : Reals.Real;

      R_2         : Reals.Real;

      R_3         : Reals.Real;

      --  Base_X     : constant := 0.074;
      --  Base_Y     : constant := 0.048;
      --  Base_Z     : constant := 0.0;
      --  Base_Gamma : constant := Ada.Numerics.Pi / 6.0;
      --  LF_Cos_Gamma_0 : constant := 0.866025404;
      --  --  Cos (Kinematics.Configuration.LF_Base_Gamma);
      --  LF_Sin_Gamma_0 : constant := 0.5;
      --  --  Sin (Kinematics.Configuration.LF_Base_Gamma);
      --  LF_Cos_Alpha_1 : constant := 0.0;
      --  --  Cos (Kinematics.Configuration.LF_DH_Alpha1);
      --  LF_Sin_Alpha_1 : constant := 1.0;
      --  --  Sin (Kinematics.Configuration.LF_DH_Alpha1);
      --  LF_Cos_Alpha_2 : constant := 1.0;
      --  --  Cos (Kinematics.Configuration.LF_DH_Alpha2);
      --  LF_Sin_Alpha_2 : constant := 0.0;
      --  --  Sin (Kinematics.Configuration.LF_DH_Alpha2);
      --  LF_Cos_Alpha_3 : constant := 1.0;
      --  --  Cos (Kinematics.Configuration.LF_DH_Alpha3);
      --  LF_Sin_Alpha_3 : constant := 0.0;
      --  --  Sin (Kinematics.Configuration.LF_DH_Alpha3);
   --  LF_DH_Alpha1 : constant := Ada.Numerics.Pi / 2.0;
   --  LF_DH_D1     : constant := 0.0;
   --  LF_DH_R1     : constant := 0.029;
   --  LF_DH_Alpha2 : constant := 0.0;
   --  LF_DH_D2     : constant := 0.0;
   --  LF_DH_R2     : constant := 0.084;
   --  LF_DH_Alpha3 : constant := 0.0;
   --  LF_DH_D3     : constant := 0.0;
   --  LF_DH_R3     : constant := 0.124;

      Workspace : Legs.Workspace;
   end record;

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
     (Self      : out Leg;
      Side      : Leg_Side;
      Base      : Leg_Base_Parameters;
      Segment_1 : Leg_Segment_Parameters;
      Segment_2 : Leg_Segment_Parameters;
      Segment_3 : Leg_Segment_Parameters);

   procedure Inverse_Kinematics
     (Self             : Leg;
      Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean);

   procedure Compute_Workspace
     (Self        : in out Leg;
      Body_Height : Reals.Real);

end Legs;
