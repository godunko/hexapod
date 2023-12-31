--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;

with Kinematics.Configuration.Derived;

package body Kinematics.Inverse.Geometric is

   procedure Solve
     (B_X         : Reals.Real;
      B_Y         : Reals.Real;
      B_Z         : Reals.Real;
      Cos_Gamma_0 : Reals.Real;
      Sin_Gamma_0 : Reals.Real;
      R_1         : Reals.Real;
      R_2         : Reals.Real;
      R_3         : Reals.Real;
      E_X         : Reals.Real;
      E_Y         : Reals.Real;
      E_Z         : Reals.Real;
      Theta_1     : out Reals.Real;
      Theta_2     : out Reals.Real;
      Theta_3     : out Reals.Real;
      Success     : out Boolean);

   --------------
   -- LF_Solve --
   --------------

   procedure LF_Solve
     (Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean)
   is
      use type Reals.Real;

      Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real;

   begin
      Solve
        (B_X         => Kinematics.Configuration.LF_Base_X,
         B_Y         => Kinematics.Configuration.LF_Base_Y,
         B_Z         => Kinematics.Configuration.LF_Base_Z,
         Cos_Gamma_0 => Kinematics.Configuration.Derived.LF_Cos_Gamma_0,
         Sin_Gamma_0 => Kinematics.Configuration.Derived.LF_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.LF_DH_R1,
         R_2         => Kinematics.Configuration.LF_DH_R2,
         R_3         => Kinematics.Configuration.LF_DH_R3,
         E_X         => Kinematics.X (Desired_Position),
         E_Y         => Kinematics.Y (Desired_Position),
         E_Z         => Kinematics.Z (Desired_Position),
         Theta_1     => Theta_1,
         Theta_2     => Theta_2,
         Theta_3     => Theta_3,
         Success     => Success);

      Set (Found_Posture, Theta_1, -Theta_2, -Theta_3);
   end LF_Solve;

   --------------
   -- LH_Solve --
   --------------

   procedure LH_Solve
     (Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean)
   is
      use type Reals.Real;

      Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real;

   begin
      Solve
        (B_X         => Kinematics.Configuration.LH_Base_X,
         B_Y         => Kinematics.Configuration.LH_Base_Y,
         B_Z         => Kinematics.Configuration.LH_Base_Z,
         Cos_Gamma_0 => Kinematics.Configuration.Derived.LH_Cos_Gamma_0,
         Sin_Gamma_0 => Kinematics.Configuration.Derived.LH_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.LH_DH_R1,
         R_2         => Kinematics.Configuration.LH_DH_R2,
         R_3         => Kinematics.Configuration.LH_DH_R3,
         E_X         => Kinematics.X (Desired_Position),
         E_Y         => Kinematics.Y (Desired_Position),
         E_Z         => Kinematics.Z (Desired_Position),
         Theta_1     => Theta_1,
         Theta_2     => Theta_2,
         Theta_3     => Theta_3,
         Success     => Success);

      Set (Found_Posture, Theta_1, -Theta_2, -Theta_3);
   end LH_Solve;

   --------------
   -- LM_Solve --
   --------------

   procedure LM_Solve
     (Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean)
   is
      use type Reals.Real;

      Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real;

   begin
      Solve
        (B_X         => Kinematics.Configuration.LM_Base_X,
         B_Y         => Kinematics.Configuration.LM_Base_Y,
         B_Z         => Kinematics.Configuration.LM_Base_Z,
         Cos_Gamma_0 => Kinematics.Configuration.Derived.LM_Cos_Gamma_0,
         Sin_Gamma_0 => Kinematics.Configuration.Derived.LM_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.LM_DH_R1,
         R_2         => Kinematics.Configuration.LM_DH_R2,
         R_3         => Kinematics.Configuration.LM_DH_R3,
         E_X         => Kinematics.X (Desired_Position),
         E_Y         => Kinematics.Y (Desired_Position),
         E_Z         => Kinematics.Z (Desired_Position),
         Theta_1     => Theta_1,
         Theta_2     => Theta_2,
         Theta_3     => Theta_3,
         Success     => Success);

      Set (Found_Posture, Theta_1, -Theta_2, -Theta_3);
   end LM_Solve;

   --------------
   -- RF_Solve --
   --------------

   procedure RF_Solve
     (Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean)
   is
      Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real;

   begin
      Solve
        (B_X         => Kinematics.Configuration.RF_Base_X,
         B_Y         => Kinematics.Configuration.RF_Base_Y,
         B_Z         => Kinematics.Configuration.RF_Base_Z,
         Cos_Gamma_0 => Kinematics.Configuration.Derived.RF_Cos_Gamma_0,
         Sin_Gamma_0 => Kinematics.Configuration.Derived.RF_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.RF_DH_R1,
         R_2         => Kinematics.Configuration.RF_DH_R2,
         R_3         => Kinematics.Configuration.RF_DH_R3,
         E_X         => Kinematics.X (Desired_Position),
         E_Y         => Kinematics.Y (Desired_Position),
         E_Z         => Kinematics.Z (Desired_Position),
         Theta_1     => Theta_1,
         Theta_2     => Theta_2,
         Theta_3     => Theta_3,
         Success     => Success);

      Set (Found_Posture, Theta_1, Theta_2, Theta_3);
   end RF_Solve;

   --------------
   -- RH_Solve --
   --------------

   procedure RH_Solve
     (Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean)
   is
      Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real;

   begin
      Solve
        (B_X         => Kinematics.Configuration.RH_Base_X,
         B_Y         => Kinematics.Configuration.RH_Base_Y,
         B_Z         => Kinematics.Configuration.RH_Base_Z,
         Cos_Gamma_0 => Kinematics.Configuration.Derived.RH_Cos_Gamma_0,
         Sin_Gamma_0 => Kinematics.Configuration.Derived.RH_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.RH_DH_R1,
         R_2         => Kinematics.Configuration.RH_DH_R2,
         R_3         => Kinematics.Configuration.RH_DH_R3,
         E_X         => Kinematics.X (Desired_Position),
         E_Y         => Kinematics.Y (Desired_Position),
         E_Z         => Kinematics.Z (Desired_Position),
         Theta_1     => Theta_1,
         Theta_2     => Theta_2,
         Theta_3     => Theta_3,
         Success     => Success);

      Set (Found_Posture, Theta_1, Theta_2, Theta_3);
   end RH_Solve;

   --------------
   -- RM_Solve --
   --------------

   procedure RM_Solve
     (Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean)
   is
      Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real;

   begin
      Solve
        (B_X         => Kinematics.Configuration.RM_Base_X,
         B_Y         => Kinematics.Configuration.RM_Base_Y,
         B_Z         => Kinematics.Configuration.RM_Base_Z,
         Cos_Gamma_0 => Kinematics.Configuration.Derived.RM_Cos_Gamma_0,
         Sin_Gamma_0 => Kinematics.Configuration.Derived.RM_Sin_Gamma_0,
         R_1         => Kinematics.Configuration.RM_DH_R1,
         R_2         => Kinematics.Configuration.RM_DH_R2,
         R_3         => Kinematics.Configuration.RM_DH_R3,
         E_X         => Kinematics.X (Desired_Position),
         E_Y         => Kinematics.Y (Desired_Position),
         E_Z         => Kinematics.Z (Desired_Position),
         Theta_1     => Theta_1,
         Theta_2     => Theta_2,
         Theta_3     => Theta_3,
         Success     => Success);

      Set (Found_Posture, Theta_1, Theta_2, Theta_3);
   end RM_Solve;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (B_X         : Reals.Real;
      B_Y         : Reals.Real;
      B_Z         : Reals.Real;
      Cos_Gamma_0 : Reals.Real;
      Sin_Gamma_0 : Reals.Real;
      R_1         : Reals.Real;
      R_2         : Reals.Real;
      R_3         : Reals.Real;
      E_X         : Reals.Real;
      E_Y         : Reals.Real;
      E_Z         : Reals.Real;
      Theta_1     : out Reals.Real;
      Theta_2     : out Reals.Real;
      Theta_3     : out Reals.Real;
      Success     : out Boolean)
   is
      use type Reals.Real;

      function Round (Value : Reals.Real) return Reals.Real;

      -----------
      -- Round --
      -----------

      function Round (Value : Reals.Real) return Reals.Real is
      begin
         if Value < -1.0 then
            if Value >= -1.0 - Reals.Real'Epsilon then
               return -1.0;

            else
               raise Constraint_Error;
            end if;

         elsif Value > 1.0 then
            if Value <= 1.0 + Reals.Real'Epsilon then
               return 1.0;

            else
               raise Constraint_Error;
            end if;

         else
            return Value;
         end if;
      end Round;

      E_X1        : constant Reals.Real :=
        Cos_Gamma_0 * (E_X - B_X) + Sin_Gamma_0 * (E_Y - B_Y);
      E_Y1        : constant Reals.Real :=
        -Sin_Gamma_0 * (E_X - B_X) + Cos_Gamma_0 * (E_Y - B_Y);
      E_Z1        : constant Reals.Real := E_Z - B_Z;
      --  Position of E in joint 1 coordinate system.
      L_E1        : constant Reals.Real :=
        Reals.Elementary_Functions.Sqrt (E_X1 * E_X1 + E_Y1 * E_Y1);
      --  Length of the projection of E1 on XY plane.

      Cos_Theta_1 : constant Reals.Real := E_X1 / L_E1;
      Sin_Theta_1 : constant Reals.Real := E_Y1 / L_E1;
      --  Directional cosines

      C2_X1       : constant Reals.Real := Cos_Theta_1 * R_1;
      C2_Y1       : constant Reals.Real := Sin_Theta_1 * R_1;
      C2_Z1       : constant Reals.Real := 0.0;
      --  Position of joint 2 in joint 1 coordinate system.

      L           : constant Reals.Real :=
        Reals.Elementary_Functions.Sqrt
          ((E_X1 - C2_X1) * (E_X1 - C2_X1)
           + (E_Y1 - C2_Y1) * (E_Y1 - C2_Y1)
           + (E_Z1 - C2_Z1) * (E_Z1 - C2_Z1));
      --  Length of the vector between E and C2

      L_XY        : constant Reals.Real :=
        Reals.Elementary_Functions.Sqrt
          ((E_X1 - C2_X1) * (E_X1 - C2_X1) + (E_Y1 - C2_Y1) * (E_Y1 - C2_Y1));

      Cos_Alpha_1 : Reals.Real;
      Cos_Alpha_2 : Reals.Real;
      Cos_Alpha_3 : Reals.Real;
      Alpha_1     : Reals.Real;
      Alpha_2     : Reals.Real;

   begin
      if abs E_X1 <= Reals.Real'Epsilon
        and abs E_Y1 <= Reals.Real'Epsilon
      then
         --  Gimbal lock: posture is independend from the rotation angle of the
         --  third joint. Solution might exists, however, ignore it for now.

         Success := False;

         return;
      end if;

      if L not in abs (R_2 - R_3) .. (R_2 + R_3) then
         --  Desired position is unreachable. No solution.

         Success := False;

         return;
      end if;

      --  Compute rotation of the first joint

      Theta_1 := Reals.Elementary_Functions.Arctan (E_Y1, E_X1);

      --  Compute rotation of the third joind

      Cos_Alpha_3 :=
        Round ((R_2 * R_2 + R_3 * R_3 - L * L) / (2.0 * R_2 * R_3));
      Theta_3     :=
        Ada.Numerics.Pi - Reals.Elementary_Functions.Arccos (Cos_Alpha_3);

      --  Compute rotation of the second joint

      Cos_Alpha_1 :=
        (L * L + L_XY * L_XY - (E_Z1 - C2_Z1) * (E_Z1 - C2_Z1))
          / (2.0 * L * L_XY);
      Alpha_1     := Reals.Elementary_Functions.Arccos (Cos_Alpha_1);
      Alpha_1     := (if E_Z1 > 0.0 then -@ else @);

      Cos_Alpha_2 := (L * L + R_2 * R_2 - R_3 * R_3) / (2.0 * L * R_2);
      Alpha_2     := Reals.Elementary_Functions.Arccos (Cos_Alpha_2);

      Theta_2 := Alpha_1 - Alpha_2;

      --  XXX Second solution is ignored.
      --     Theta_2 := Alpha_1 + Alpha_2

      Success := True;
   end Solve;

end Kinematics.Inverse.Geometric;
