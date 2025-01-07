--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  pragma Restrictions (No_Elaboration_Code);

with Kinematics.Forward;
with Kinematics.Inverse.Geometric;

package body Legs is

   ------------------------
   -- Forward_Kinematics --
   ------------------------

   function Forward_Kinematics
     (Self    : Leg;
      Posture : Kinematics.Posture) return Kinematics.Position is
   begin
      case Self.Index is
         when Left_Front =>
            return Kinematics.Forward.LF_E_Position (Posture);
         when Left_Middle =>
            return Kinematics.Forward.LM_E_Position (Posture);
         when Left_Hind =>
            return Kinematics.Forward.LH_E_Position (Posture);
         when Right_Front =>
            return Kinematics.Forward.RF_E_Position (Posture);
         when Right_Middle =>
            return Kinematics.Forward.RM_E_Position (Posture);
         when Right_Hind =>
            return Kinematics.Forward.RH_E_Position (Posture);
      end case;
   end Forward_Kinematics;

   ------------------------
   -- Forward_Kinematics --
   ------------------------

   procedure Forward_Kinematics
     (Self     : Leg_Kinematics_Parameters;
      Posture  : Kinematics.Posture;
      Base     : out CGK.Primitives.Points_3D.Point_3D;
      Joint_1  : out CGK.Primitives.Points_3D.Point_3D;
      Joint_2  : out CGK.Primitives.Points_3D.Point_3D;
      Joint_3  : out CGK.Primitives.Points_3D.Point_3D;
      Effector : out CGK.Primitives.Points_3D.Point_3D)
   is
      Origin : constant CGK.Primitives.Points_3D.Point_3D :=
        CGK.Primitives.Points_3D.As_Point_3D (0.0, 0.0, 0.0);

      T      : CGK.Primitives.Transformations_3D.Transformation_3D;
      T_1_2  : CGK.Primitives.Transformations_3D.Transformation_3D;
      T_2_3  : CGK.Primitives.Transformations_3D.Transformation_3D;
      T_3_E  : CGK.Primitives.Transformations_3D.Transformation_3D;

   begin
      Base     := Origin;
      Joint_1  := Origin;
      Joint_2  := Origin;
      Joint_3  := Origin;
      Effector := Origin;

      CGK.Primitives.Transformations_3D.Set_Denavit_Hartenberg
        (Self => T_1_2,
         d    => Self.D_1,
         θ    => Kinematics.Theta_1 (Posture),
         r    => Self.R_1,
         α    => Self.α_1);
      CGK.Primitives.Transformations_3D.Set_Denavit_Hartenberg
        (Self => T_2_3,
         d    => Self.D_2,
         θ    => Kinematics.Theta_2 (Posture),
         r    => Self.R_2,
         α    => Self.α_2);
      CGK.Primitives.Transformations_3D.Set_Denavit_Hartenberg
        (Self => T_3_E,
         d    => Self.D_3,
         θ    => Kinematics.Theta_3 (Posture),
         r    => Self.R_3,
         α    => Self.α_3);

      T := Self.T_B_1;
      CGK.Primitives.Points_3D.Transform (Joint_1, T);

      CGK.Primitives.Transformations_3D.Multiply (T, T_1_2);
      CGK.Primitives.Points_3D.Transform (Joint_2, T);

      CGK.Primitives.Transformations_3D.Multiply (T, T_2_3);
      CGK.Primitives.Points_3D.Transform (Joint_3, T);

      CGK.Primitives.Transformations_3D.Multiply (T, T_3_E);
      CGK.Primitives.Points_3D.Transform (Effector, T);
   end Forward_Kinematics;

   ------------------------
   -- Inverse_Kinematics --
   ------------------------

   procedure Inverse_Kinematics
     (Self             : Leg_Kinematics_Parameters;
      Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean) is
   begin
      Kinematics.Inverse.Geometric.Solve
        (B_X              => Self.X_0,
         B_Y              => Self.Y_0,
         B_Z              => Self.Z_0,
         Cos_Gamma_0      => Self.Cos_Gamma_0,
         Sin_Gamma_0      => Self.Sin_Gamma_0,
         R_1              => Self.R_1,
         R_2              => Self.R_2,
         R_3              => Self.R_3,
         Inverse          => Self.Side = Left,
         Desired_Position => Desired_Position,
         Found_Posture    => Found_Posture,
         Success          => Success);
   end Inverse_Kinematics;

end Legs;
