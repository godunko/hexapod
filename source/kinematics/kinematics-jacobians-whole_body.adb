--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with CGK.Reals.Elementary_Functions;

package body Kinematics.Jacobians.Whole_Body is

   procedure Compute_Jacobian
     (Posture : Kinematics.Posture;
      Compute : not null Compute_Leg_Jacobian;
      M_11    : out CGK.Reals.Real;
      M_12    : out CGK.Reals.Real;
      M_13    : out CGK.Reals.Real;
      M_21    : out CGK.Reals.Real;
      M_22    : out CGK.Reals.Real;
      M_23    : out CGK.Reals.Real;
      M_31    : out CGK.Reals.Real;
      M_32    : out CGK.Reals.Real;
      M_33    : out CGK.Reals.Real) with Inline_Always;

   ----------------------
   -- Compute_Jacobian --
   ----------------------

   procedure Compute_Jacobian
     (LF_Posture : Kinematics.Posture;
      LM_Posture : Kinematics.Posture;
      LH_Posture : Kinematics.Posture;
      RF_Posture : Kinematics.Posture;
      RM_Posture : Kinematics.Posture;
      RH_Posture : Kinematics.Posture;
      Result     : out Matrix)
   is
      Offset : Natural := 0;

   begin
      Compute_Jacobian
        (Posture => LF_Posture,
         Compute => LF'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;

      Compute_Jacobian
        (Posture => LM_Posture,
         Compute => LM'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;

      Compute_Jacobian
        (Posture => LH_Posture,
         Compute => LH'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;

      Compute_Jacobian
        (Posture => RF_Posture,
         Compute => RF'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;

      Compute_Jacobian
        (Posture => RM_Posture,
         Compute => RM'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;

      Compute_Jacobian
        (Posture => RH_Posture,
         Compute => RH'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;
   end Compute_Jacobian;

   ----------------------
   -- Compute_Jacobian --
   ----------------------

   procedure Compute_Jacobian
     (Posture : Kinematics.Posture;
      Compute : not null Compute_Leg_Jacobian;
      M_11    : out CGK.Reals.Real;
      M_12    : out CGK.Reals.Real;
      M_13    : out CGK.Reals.Real;
      M_21    : out CGK.Reals.Real;
      M_22    : out CGK.Reals.Real;
      M_23    : out CGK.Reals.Real;
      M_31    : out CGK.Reals.Real;
      M_32    : out CGK.Reals.Real;
      M_33    : out CGK.Reals.Real)
   is
      Theta_1 : constant CGK.Reals.Real := Kinematics.Theta_1 (Posture);
      Theta_2 : constant CGK.Reals.Real := Kinematics.Theta_2 (Posture);
      Theta_3 : constant CGK.Reals.Real := Kinematics.Theta_3 (Posture);

      Cos_Theta_1 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Cos (Theta_1);
      Sin_Theta_1 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Sin (Theta_1);
      Cos_Theta_2 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Cos (Theta_2);
      Sin_Theta_2 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Sin (Theta_2);
      Cos_Theta_3 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Cos (Theta_3);
      Sin_Theta_3 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Sin (Theta_3);

   begin
      Compute
        (Cos_Theta_1 => Cos_Theta_1,
         Sin_Theta_1 => Sin_Theta_1,
         Cos_Theta_2 => Cos_Theta_2,
         Sin_Theta_2 => Sin_Theta_2,
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         M_11        => M_11,
         M_12        => M_12,
         M_13        => M_13,
         M_21        => M_21,
         M_22        => M_22,
         M_23        => M_23,
         M_31        => M_31,
         M_32        => M_32,
         M_33        => M_33);
   end Compute_Jacobian;

end Kinematics.Jacobians.Whole_Body;
