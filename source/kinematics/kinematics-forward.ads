--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Computes basic matricies of Forward Kinematics. Configuration parameters
--  are defined in the Kinematics.Configuration package.

pragma Restrictions (No_Elaboration_Code);

with Reals;

package Kinematics.Forward is

   pragma Pure;

   procedure LF_T_BE
     (Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real;
      Result  : out Reals.Transformations_3D.Transformation_3D);
   --  Create transformation to compute position of the end effector.

   function LF_E_Position
     (Posture : Kinematics.Posture) return Reals.Vectors_3D.Vector_3D;
   --  Returns position of the end effector in the body coordinate system with
   --  specified angles.

   function LF_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position;
   --  Returns position of the end effector in the body coordinate system with
   --  specified angles.

   function LM_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position;
   --  Returns position of the end effector in the body coordinate system with
   --  specified angles.

   function LH_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position;
   --  Returns position of the end effector in the body coordinate system with
   --  specified angles.

   function RF_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position;
   --  Returns position of the end effector in the body coordinate system with
   --  specified angles.

   function RM_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position;
   --  Returns position of the end effector in the body coordinate system with
   --  specified angles.

   function RH_E_Position
     (Posture : Kinematics.Posture) return Kinematics.Position;
   --  Returns position of the end effector in the body coordinate system with
   --  specified angles.

end Kinematics.Forward;
