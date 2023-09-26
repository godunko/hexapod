--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Configuration parameters of the hexapod's kinematics library.
--
--  Default values corresponds to Phoenyx Hexapod on ALiExpress.

pragma Restrictions (No_Elaboration_Code);

with Ada.Numerics;

package Kinematics.Configuration is

   pragma Pure;

   --  Leg base positions, X, Y, X are coordinates and Alpha is angle of
   --  rotation around Z axis.

   LF_Base_X     : constant := 0.074;
   LF_Base_Y     : constant := 0.048;
   LF_Base_Z     : constant := 0.0;
   LF_Base_Alpha : constant := 0.0;

   LM_Base_X     : constant := 0.0;
   LM_Base_Y     : constant := 0.065;
   LM_Base_Z     : constant := 0.0;
   LM_Base_Alpha : constant := Ada.Numerics.Pi / 2.0;

   LR_Base_X     : constant := -0.074;
   LR_Base_Y     : constant := -0.048;
   LR_Base_Z     : constant := 0.0;
   LR_Base_Alpha : constant := Ada.Numerics.Pi;

   RF_Base_X     : constant := 0.074;
   RF_Base_Y     : constant := -0.048;
   RF_Base_Z     : constant := 0.0;
   RF_Base_Alpha : constant := 0.0;

   RM_Base_X     : constant := 0.0;
   RM_Base_Y     : constant := -0.065;
   RM_Base_Z     : constant := 0.0;
   RM_Base_Alpha : constant := -Ada.Numerics.Pi / 2.0;

   RR_Base_X     : constant := -0.074;
   RR_Base_Y     : constant := -0.048;
   RR_Base_Z     : constant := 0.0;
   RR_Base_Alpha : constant := -Ada.Numerics.Pi;

   --  DH parameters of the frames.

   LF_DH_R1     : constant := 0.029;
   LF_DH_Alpha1 : constant := Ada.Numerics.Pi / 2.0;
   LF_DH_R2     : constant := 0.084;
   LF_DH_Alpha2 : constant := 0.0;
   LF_DH_R3     : constant := 0.124;
   LF_DH_Alpha3 : constant := 0.0;

end Kinematics.Configuration;
