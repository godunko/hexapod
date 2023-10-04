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

end Kinematics.Configuration.Derived;
