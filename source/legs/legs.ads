--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with Kinematics;
with Reals;

package Legs
  with Preelaborate
is

   type Leg_Side is (Left, Right);

   type Leg_Information is record
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
   --  LF_DH_Alpha2 : constant := 0.0;
   --  LF_DH_D2     : constant := 0.0;
   --  LF_DH_Alpha3 : constant := 0.0;
   --  LF_DH_D3     : constant := 0.0;

      --  Workspace   : Legs.Workspace;
   end record;

   type Leg_Index is
     (Right_Front, Right_Middle, Right_Rear,
      Left_Rear, Left_Middle, Left_Front);
   --  Leg's indicies.
   --
   --  Legs are listed in counter-clockwise order to avoid use of the mapping
   --  by the free gait generator.

   Legs : array (Leg_Index) of Leg_Information;

   procedure Initialize;

   procedure Inverse_Kinematics
     (Self             : Leg_Information;
      Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean);

end Legs;
