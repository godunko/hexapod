--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Kinematics.Configuration.Derived;
with Kinematics.Inverse.Algebraic.Generic_Compute_12;
with Kinematics.Inverse.Algebraic.Generic_Compute_T;

package body Kinematics.Inverse.Algebraic is

   procedure RF_Compute_12 is
     new Kinematics.Inverse.Algebraic.Generic_Compute_12
       (B_X         => Kinematics.Configuration.RF_Base_X,
        B_Y         => Kinematics.Configuration.RF_Base_Y,
        B_Z         => Kinematics.Configuration.RF_Base_Z,
        Cos_Gamma_0 => Kinematics.Configuration.Derived.RF_Cos_Gamma_0,
        Sin_Gamma_0 => Kinematics.Configuration.Derived.RF_Sin_Gamma_0,
        Cos_Alpha_1 => Kinematics.Configuration.Derived.RF_Cos_Alpha_1,
        Sin_Alpha_1 => Kinematics.Configuration.Derived.RF_Sin_Alpha_1,
        Cos_Alpha_2 => Kinematics.Configuration.Derived.RF_Cos_Alpha_2,
        Sin_Alpha_2 => Kinematics.Configuration.Derived.RF_Sin_Alpha_2,
        D_1         => Kinematics.Configuration.RF_DH_D1,
        D_2         => Kinematics.Configuration.RF_DH_D2,
        D_3         => Kinematics.Configuration.RF_DH_D3,
        R_1         => Kinematics.Configuration.RF_DH_R1,
        R_2         => Kinematics.Configuration.RF_DH_R2,
        R_3         => Kinematics.Configuration.RF_DH_R3);

   procedure RF_Compute_T is
     new Kinematics.Inverse.Algebraic.Generic_Compute_T
       (B_X => Kinematics.Configuration.RF_Base_X,
        B_Y => Kinematics.Configuration.RF_Base_Y,
        R_1 => Kinematics.Configuration.RF_DH_R1,
        R_2 => Kinematics.Configuration.RF_DH_R2,
        R_3 => Kinematics.Configuration.RF_DH_R3);

   --------------
   -- LF_Solve --
   --------------

   procedure LF_Solve
     (Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean) is
   begin
      null;
   end LF_Solve;

   --------------
   -- RF_Solve --
   --------------

   procedure RF_Solve
     (Position : Kinematics.Position;
      Solution : out Algebraic_Solution_Array;
      Found    : out Natural)
   is
      procedure Cos_Sin_3
        (T       : Reals.Real;
         Cos_3   : out Reals.Real;
         Sin_3   : out Reals.Real;
         Theta_3 : out Reals.Real);

      ---------------
      -- Cos_Sin_3 --
      ---------------

      procedure Cos_Sin_3
        (T       : Reals.Real;
         Cos_3   : out Reals.Real;
         Sin_3   : out Reals.Real;
         Theta_3 : out Reals.Real)
      is
         use type Reals.Real;

      begin
         Cos_3   := (1.0 - T ** 2) / (1.0 + T ** 2);
         Sin_3   := 2.0 * T / (1.0 + T ** 2);
         Theta_3 := 2.0 * Reals.Elementary_Functions.Arctan (T);
      end Cos_Sin_3;

      T_1         : Reals.Real;
      T_2         : Reals.Real;
      T_3         : Reals.Real;
      T_4         : Reals.Real;
      Theta_1     : Reals.Real;
      Theta_2     : Reals.Real;
      Cos_Theta_3 : Reals.Real;
      Sin_Theta_3 : Reals.Real;
      Theta_3     : Reals.Real;

   begin
      RF_Compute_T
        (E_X   => Kinematics.X (Position),
         E_Y   => Kinematics.Y (Position),
         E_Z   => Kinematics.Z (Position),
         T_1   => T_1,
         T_2   => T_2,
         T_3   => T_3,
         T_4   => T_4,
         Count => Found);

      if Found = 0 then
         return;
      end if;

      Cos_Sin_3 (T_1, Cos_Theta_3, Sin_Theta_3, Theta_3);
      RF_Compute_12
        (E_X         => Kinematics.X (Position),
         E_Y         => Kinematics.Y (Position),
         E_Z         => Kinematics.Z (Position),
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         Theta_1     => Theta_1,
         Theta_2     => Theta_2);
      Solution (1) := (Exists => True, others => <>);
      Kinematics.Set (Solution (1).Posture, Theta_1, Theta_2, Theta_3);

      if Found = 1 then
         return;
      end if;

      Cos_Sin_3 (T_2, Cos_Theta_3, Sin_Theta_3, Theta_3);
      RF_Compute_12
        (E_X         => Kinematics.X (Position),
         E_Y         => Kinematics.Y (Position),
         E_Z         => Kinematics.Z (Position),
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         Theta_1     => Theta_1,
         Theta_2     => Theta_2);
      Solution (2) := (Exists => True, others => <>);
      Kinematics.Set (Solution (2).Posture, Theta_1, Theta_2, Theta_3);

      if Found = 2 then
         return;
      end if;

      Cos_Sin_3 (T_3, Cos_Theta_3, Sin_Theta_3, Theta_3);
      RF_Compute_12
        (E_X         => Kinematics.X (Position),
         E_Y         => Kinematics.Y (Position),
         E_Z         => Kinematics.Z (Position),
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         Theta_1     => Theta_1,
         Theta_2     => Theta_2);
      Solution (3) := (Exists => True, others => <>);
      Kinematics.Set (Solution (3).Posture, Theta_1, Theta_2, Theta_3);

      if Found = 3 then
         return;
      end if;

      Cos_Sin_3 (T_4, Cos_Theta_3, Sin_Theta_3, Theta_3);
      RF_Compute_12
        (E_X         => Kinematics.X (Position),
         E_Y         => Kinematics.Y (Position),
         E_Z         => Kinematics.Z (Position),
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         Theta_1     => Theta_1,
         Theta_2     => Theta_2);
      Solution (4) := (Exists => True, others => <>);
      Kinematics.Set (Solution (4).Posture, Theta_1, Theta_2, Theta_3);
   end RF_Solve;

end Kinematics.Inverse.Algebraic;
