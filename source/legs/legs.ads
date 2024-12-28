--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  pragma Restrictions (No_Elaboration_Code);

with CGK.Primitives.Points_3D;
with CGK.Primitives.Transformations_3D;

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

      D_1         : Reals.Real;
      R_1         : Reals.Real;
      α_1         : Reals.Real;

      D_2         : Reals.Real;
      R_2         : Reals.Real;
      α_2         : Reals.Real;

      D_3         : Reals.Real;
      R_3         : Reals.Real;
      α_3         : Reals.Real;

      --  Workspace   : Legs.Workspace;

      T_B_1       : CGK.Primitives.Transformations_3D.Transformation_3D;
   end record;

   type Leg_Index is
     (Right_Front, Right_Middle, Right_Hind,
      Left_Hind, Left_Middle, Left_Front);
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

   procedure Forward_Kinematics
     (Self     : Leg_Information;
      Posture  : Kinematics.Posture;
      Base     : out CGK.Primitives.Points_3D.Point_3D;
      Joint_1  : out CGK.Primitives.Points_3D.Point_3D;
      Joint_2  : out CGK.Primitives.Points_3D.Point_3D;
      Joint_3  : out CGK.Primitives.Points_3D.Point_3D;
      Effector : out CGK.Primitives.Points_3D.Point_3D);
   --  Computes forward kinematics and returns coordintes of base, all joints,
   --  and end effector.

end Legs;
