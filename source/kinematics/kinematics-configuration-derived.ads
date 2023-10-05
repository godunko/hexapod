--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Derived configuration parameters of the hexapod's kinematics library. All
--  values in this package are precomuted from the corresponding elements of
--  parent package.

pragma Restrictions (No_Elaboration_Code);

package Kinematics.Configuration.Derived is

   pragma Pure;

   LF_Cos_Gamma_0 : constant := 0.866025404;
   --  Cos (Kinematics.Configuration.LF_Base_Gamma);
   LF_Sin_Gamma_0 : constant := 0.5;
   --  Sin (Kinematics.Configuration.LF_Base_Gamma);
   LF_Cos_Alpha_1 : constant := 0.0;
   --  Cos (Kinematics.Configuration.LF_DH_Alpha1);
   LF_Sin_Alpha_1 : constant := 1.0;
   --  Sin (Kinematics.Configuration.LF_DH_Alpha1);
   LF_Cos_Alpha_2 : constant := 1.0;
   --  Cos (Kinematics.Configuration.LF_DH_Alpha2);
   LF_Sin_Alpha_2 : constant := 0.0;
   --  Sin (Kinematics.Configuration.LF_DH_Alpha2);
   LF_Cos_Alpha_3 : constant := 1.0;
   --  Cos (Kinematics.Configuration.LF_DH_Alpha3);
   LF_Sin_Alpha_3 : constant := 0.0;
   --  Sin (Kinematics.Configuration.LF_DH_Alpha3);

   LM_Cos_Gamma_0 : constant := 0.0;
   --  Cos (Kinematics.Configuration.LF_Base_Gamma);
   LM_Sin_Gamma_0 : constant := 1.0;
   --  Sin (Kinematics.Configuration.LF_Base_Gamma);
   LM_Cos_Alpha_1 : constant := 0.0;
   --  Cos (Kinematics.Configuration.LF_DH_Alpha1);
   LM_Sin_Alpha_1 : constant := 1.0;
   --  Sin (Kinematics.Configuration.LF_DH_Alpha1);
   LM_Cos_Alpha_2 : constant := 1.0;
   --  Cos (Kinematics.Configuration.LF_DH_Alpha2);
   LM_Sin_Alpha_2 : constant := 0.0;
   --  Sin (Kinematics.Configuration.LF_DH_Alpha2);
   LM_Cos_Alpha_3 : constant := 1.0;
   --  Cos (Kinematics.Configuration.LF_DH_Alpha3);
   LM_Sin_Alpha_3 : constant := 0.0;
   --  Sin (Kinematics.Configuration.LF_DH_Alpha3);

   LR_Cos_Gamma_0 : constant := -0.866025404;
   --  Cos (Kinematics.Configuration.LF_Base_Gamma);
   LR_Sin_Gamma_0 : constant := 0.5;
   --  Sin (Kinematics.Configuration.LF_Base_Gamma);
   LR_Cos_Alpha_1 : constant := 0.0;
   --  Cos (Kinematics.Configuration.LF_DH_Alpha1);
   LR_Sin_Alpha_1 : constant := 1.0;
   --  Sin (Kinematics.Configuration.LF_DH_Alpha1);
   LR_Cos_Alpha_2 : constant := 1.0;
   --  Cos (Kinematics.Configuration.LF_DH_Alpha2);
   LR_Sin_Alpha_2 : constant := 0.0;
   --  Sin (Kinematics.Configuration.LF_DH_Alpha2);
   LR_Cos_Alpha_3 : constant := 1.0;
   --  Cos (Kinematics.Configuration.LF_DH_Alpha3);
   LR_Sin_Alpha_3 : constant := 0.0;
   --  Sin (Kinematics.Configuration.LF_DH_Alpha3);

   RF_Cos_Gamma_0 : constant := 0.866025404;
   --  Cos (Kinematics.Configuration.RF_Base_Gamma);
   RF_Sin_Gamma_0 : constant := -0.5;
   --  Sin (Kinematics.Configuration.RF_Base_Gamma);
   RF_Cos_Alpha_1 : constant := 0.0;
   --  Cos (Kinematics.Configuration.RF_DH_Alpha1);
   RF_Sin_Alpha_1 : constant := -1.0;
   --  Sin (Kinematics.Configuration.RF_DH_Alpha1);
   RF_Cos_Alpha_2 : constant := 1.0;
   --  Cos (Kinematics.Configuration.RF_DH_Alpha2);
   RF_Sin_Alpha_2 : constant := 0.0;
   --  Sin (Kinematics.Configuration.RF_DH_Alpha2);
   RF_Cos_Alpha_3 : constant := 1.0;
   --  Cos (Kinematics.Configuration.RF_DH_Alpha3);
   RF_Sin_Alpha_3 : constant := 0.0;
   --  Sin (Kinematics.Configuration.RF_DH_Alpha3);

   RM_Cos_Gamma_0 : constant := 0.0;
   --  Cos (Kinematics.Configuration.RF_Base_Gamma);
   RM_Sin_Gamma_0 : constant := -1.0;
   --  Sin (Kinematics.Configuration.RF_Base_Gamma);
   RM_Cos_Alpha_1 : constant := 0.0;
   --  Cos (Kinematics.Configuration.RF_DH_Alpha1);
   RM_Sin_Alpha_1 : constant := -1.0;
   --  Sin (Kinematics.Configuration.RF_DH_Alpha1);
   RM_Cos_Alpha_2 : constant := 1.0;
   --  Cos (Kinematics.Configuration.RF_DH_Alpha2);
   RM_Sin_Alpha_2 : constant := 0.0;
   --  Sin (Kinematics.Configuration.RF_DH_Alpha2);
   RM_Cos_Alpha_3 : constant := 1.0;
   --  Cos (Kinematics.Configuration.RF_DH_Alpha3);
   RM_Sin_Alpha_3 : constant := 0.0;
   --  Sin (Kinematics.Configuration.RF_DH_Alpha3);

   RR_Cos_Gamma_0 : constant := -0.866025404;
   --  Cos (Kinematics.Configuration.RF_Base_Gamma);
   RR_Sin_Gamma_0 : constant := -0.5;
   --  Sin (Kinematics.Configuration.RF_Base_Gamma);
   RR_Cos_Alpha_1 : constant := 0.0;
   --  Cos (Kinematics.Configuration.RF_DH_Alpha1);
   RR_Sin_Alpha_1 : constant := -1.0;
   --  Sin (Kinematics.Configuration.RF_DH_Alpha1);
   RR_Cos_Alpha_2 : constant := 1.0;
   --  Cos (Kinematics.Configuration.RF_DH_Alpha2);
   RR_Sin_Alpha_2 : constant := 0.0;
   --  Sin (Kinematics.Configuration.RF_DH_Alpha2);
   RR_Cos_Alpha_3 : constant := 1.0;
   --  Cos (Kinematics.Configuration.RF_DH_Alpha3);
   RR_Sin_Alpha_3 : constant := 0.0;
   --  Sin (Kinematics.Configuration.RF_DH_Alpha3);

end Kinematics.Configuration.Derived;
