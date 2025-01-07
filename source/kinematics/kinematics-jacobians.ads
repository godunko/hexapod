--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with Kinematics.Configuration.Derived;
with Kinematics.Generic_Linear_Velocity_Jacobian;

package Kinematics.Jacobians
  with Pure
is

   type Compute_Leg_Jacobian is
     access procedure
       (Cos_Theta_1 : CGK.Reals.Real;
        Sin_Theta_1 : CGK.Reals.Real;
        Cos_Theta_2 : CGK.Reals.Real;
        Sin_Theta_2 : CGK.Reals.Real;
        Cos_Theta_3 : CGK.Reals.Real;
        Sin_Theta_3 : CGK.Reals.Real;
        M_11        : out CGK.Reals.Real;
        M_12        : out CGK.Reals.Real;
        M_13        : out CGK.Reals.Real;
        M_21        : out CGK.Reals.Real;
        M_22        : out CGK.Reals.Real;
        M_23        : out CGK.Reals.Real;
        M_31        : out CGK.Reals.Real;
        M_32        : out CGK.Reals.Real;
        M_33        : out CGK.Reals.Real);

   procedure LF is
     new Kinematics.Generic_Linear_Velocity_Jacobian
       (Cos_Base_Gamma => Kinematics.Configuration.Derived.LF_Cos_Gamma_0,
        Sin_Base_Gamma => Kinematics.Configuration.Derived.LF_Sin_Gamma_0,
        R_1            => Kinematics.Configuration.LF_DH_R1,
        Cos_Alpha_1    => Kinematics.Configuration.Derived.LF_Cos_Alpha_1,
        Sin_Alpha_1    => Kinematics.Configuration.Derived.LF_Sin_Alpha_1,
        D_2            => Kinematics.Configuration.LF_DH_D2,
        R_2            => Kinematics.Configuration.LF_DH_R2,
        Cos_Alpha_2    => Kinematics.Configuration.Derived.LF_Cos_Alpha_2,
        Sin_Alpha_2    => Kinematics.Configuration.Derived.LF_Sin_Alpha_2,
        D_3            => Kinematics.Configuration.LF_DH_D3,
        R_3            => Kinematics.Configuration.LF_DH_R2);

   procedure LM is
     new Kinematics.Generic_Linear_Velocity_Jacobian
       (Cos_Base_Gamma => Kinematics.Configuration.Derived.LM_Cos_Gamma_0,
        Sin_Base_Gamma => Kinematics.Configuration.Derived.LM_Sin_Gamma_0,
        R_1            => Kinematics.Configuration.LM_DH_R1,
        Cos_Alpha_1    => Kinematics.Configuration.Derived.LM_Cos_Alpha_1,
        Sin_Alpha_1    => Kinematics.Configuration.Derived.LM_Sin_Alpha_1,
        D_2            => Kinematics.Configuration.LM_DH_D2,
        R_2            => Kinematics.Configuration.LM_DH_R2,
        Cos_Alpha_2    => Kinematics.Configuration.Derived.LM_Cos_Alpha_2,
        Sin_Alpha_2    => Kinematics.Configuration.Derived.LM_Sin_Alpha_2,
        D_3            => Kinematics.Configuration.LM_DH_D3,
        R_3            => Kinematics.Configuration.LM_DH_R2);

   procedure LH is
     new Kinematics.Generic_Linear_Velocity_Jacobian
       (Cos_Base_Gamma => Kinematics.Configuration.Derived.LH_Cos_Gamma_0,
        Sin_Base_Gamma => Kinematics.Configuration.Derived.LH_Sin_Gamma_0,
        R_1            => Kinematics.Configuration.LH_DH_R1,
        Cos_Alpha_1    => Kinematics.Configuration.Derived.LH_Cos_Alpha_1,
        Sin_Alpha_1    => Kinematics.Configuration.Derived.LH_Sin_Alpha_1,
        D_2            => Kinematics.Configuration.LH_DH_D2,
        R_2            => Kinematics.Configuration.LH_DH_R2,
        Cos_Alpha_2    => Kinematics.Configuration.Derived.LH_Cos_Alpha_2,
        Sin_Alpha_2    => Kinematics.Configuration.Derived.LH_Sin_Alpha_2,
        D_3            => Kinematics.Configuration.LH_DH_D3,
        R_3            => Kinematics.Configuration.LH_DH_R2);

   procedure RF is
     new Kinematics.Generic_Linear_Velocity_Jacobian
       (Cos_Base_Gamma => Kinematics.Configuration.Derived.RF_Cos_Gamma_0,
        Sin_Base_Gamma => Kinematics.Configuration.Derived.RF_Sin_Gamma_0,
        R_1            => Kinematics.Configuration.RF_DH_R1,
        Cos_Alpha_1    => Kinematics.Configuration.Derived.RF_Cos_Alpha_1,
        Sin_Alpha_1    => Kinematics.Configuration.Derived.RF_Sin_Alpha_1,
        D_2            => Kinematics.Configuration.RF_DH_D2,
        R_2            => Kinematics.Configuration.RF_DH_R2,
        Cos_Alpha_2    => Kinematics.Configuration.Derived.RF_Cos_Alpha_2,
        Sin_Alpha_2    => Kinematics.Configuration.Derived.RF_Sin_Alpha_2,
        D_3            => Kinematics.Configuration.RF_DH_D3,
        R_3            => Kinematics.Configuration.RF_DH_R2);

   procedure RM is
     new Kinematics.Generic_Linear_Velocity_Jacobian
       (Cos_Base_Gamma => Kinematics.Configuration.Derived.RM_Cos_Gamma_0,
        Sin_Base_Gamma => Kinematics.Configuration.Derived.RM_Sin_Gamma_0,
        R_1            => Kinematics.Configuration.RM_DH_R1,
        Cos_Alpha_1    => Kinematics.Configuration.Derived.RM_Cos_Alpha_1,
        Sin_Alpha_1    => Kinematics.Configuration.Derived.RM_Sin_Alpha_1,
        D_2            => Kinematics.Configuration.RM_DH_D2,
        R_2            => Kinematics.Configuration.RM_DH_R2,
        Cos_Alpha_2    => Kinematics.Configuration.Derived.RM_Cos_Alpha_2,
        Sin_Alpha_2    => Kinematics.Configuration.Derived.RM_Sin_Alpha_2,
        D_3            => Kinematics.Configuration.RM_DH_D3,
        R_3            => Kinematics.Configuration.RM_DH_R2);

   procedure RH is
     new Kinematics.Generic_Linear_Velocity_Jacobian
       (Cos_Base_Gamma => Kinematics.Configuration.Derived.RH_Cos_Gamma_0,
        Sin_Base_Gamma => Kinematics.Configuration.Derived.RH_Sin_Gamma_0,
        R_1            => Kinematics.Configuration.RH_DH_R1,
        Cos_Alpha_1    => Kinematics.Configuration.Derived.RH_Cos_Alpha_1,
        Sin_Alpha_1    => Kinematics.Configuration.Derived.RH_Sin_Alpha_1,
        D_2            => Kinematics.Configuration.RH_DH_D2,
        R_2            => Kinematics.Configuration.RH_DH_R2,
        Cos_Alpha_2    => Kinematics.Configuration.Derived.RH_Cos_Alpha_2,
        Sin_Alpha_2    => Kinematics.Configuration.Derived.RH_Sin_Alpha_2,
        D_3            => Kinematics.Configuration.RH_DH_D3,
        R_3            => Kinematics.Configuration.RH_DH_R2);

end Kinematics.Jacobians;
