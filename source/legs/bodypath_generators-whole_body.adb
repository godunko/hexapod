--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with CGK.Primitives.Points_3D;
with CGK.Reals;

with Hexapod.Parameters.Control_Cycle;
with Legs.State;

package body Bodypath_Generators.Whole_Body is

   J          : Kinematics.Jacobians.Whole_Body.Matrix_18x18;
   I          : Kinematics.Jacobians.Whole_Body.Matrix_18x18;
   MA         : Kinematics.Jacobians.Whole_Body.Matrix_18x18;
   J1         : Kinematics.Jacobians.Whole_Body.Matrix_18x18;

   JX1        : Kinematics.Jacobians.Whole_Body.Matrix_18x6;
   JX2        : Kinematics.Jacobians.Whole_Body.Matrix_6x6;
   J1_JX1     : Kinematics.Jacobians.Whole_Body.Matrix_18x6;
   J1_JX1_JX2 : Kinematics.Jacobians.Whole_Body.Matrix_18x6;

   ----------
   -- Tick --
   ----------

   overriding procedure Tick
     (Self : in out Whole_Body_Bodypath_Generator)
   is
      use type Kinematics.Jacobians.Whole_Body.Vector_18;
      use type CGK.Reals.Real;

      Velocity : Kinematics.Jacobians.Whole_Body.Vector_6;

   begin
      Velocity :=
        [Kinematics.Linear_X (Self.Velocity),
         Kinematics.Linear_Y (Self.Velocity),
         Kinematics.Linear_Z (Self.Velocity),
         Kinematics.Angular_X (Self.Velocity),
         Kinematics.Angular_Y (Self.Velocity),
         -Kinematics.Angular_Z (Self.Velocity)];

      Kinematics.Jacobians.Whole_Body.Compute_Jacobian
        (LF_Posture =>
           Legs.State.Legs (Legs.Left_Front).Configuration.Posture,
         LM_Posture =>
           Legs.State.Legs (Legs.Left_Middle).Configuration.Posture,
         LH_Posture =>
           Legs.State.Legs (Legs.Left_Hind).Configuration.Posture,
         RF_Posture =>
           Legs.State.Legs (Legs.Right_Front).Configuration.Posture,
         RM_Posture =>
           Legs.State.Legs (Legs.Right_Middle).Configuration.Posture,
         RH_Posture =>
           Legs.State.Legs (Legs.Right_Hind).Configuration.Posture,
         Result     => J);
      Kinematics.Jacobians.Whole_Body.Inverse (J, I, MA, J1);

      Kinematics.Jacobians.Whole_Body.JX1 (0.0, 0.0, 0.0, JX1);
      Kinematics.Jacobians.Whole_Body.Matrix_Matrix_Product (J1, JX1, J1_JX1);

      Kinematics.Jacobians.Whole_Body.JX2 (0.0, 0.0, JX2);
      Kinematics.Jacobians.Whole_Body.Matrix_Matrix_Product
        (J1_JX1, JX2, J1_JX1_JX2);

      Kinematics.Jacobians.Whole_Body.Matrix_Vector_Product
        (J1_JX1_JX2, Velocity, Self.Configuration);

      Self.Configuration :=
        -@ * Hexapod.Parameters.Control_Cycle.Tick_Duration;
   end Tick;

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (Self     : Whole_Body_Bodypath_Generator'Class;
      Position : in out Kinematics.Position) is
   begin
      CGK.Primitives.Points_3D.Transform (Position, Self.Transformation);
   end Transform;

   ------------
   -- Update --
   ------------

   procedure Update
     (Self    : Whole_Body_Bodypath_Generator'Class;
      Leg     : Legs.Leg_Index;
      Posture : in out Kinematics.Posture)
   is
      use type CGK.Reals.Real;

      D1 : CGK.Reals.Real;
      D2 : CGK.Reals.Real;
      D3 : CGK.Reals.Real;

   begin
      case Leg is
         when Legs.Left_Front =>
            D1 := Self.Configuration (1);
            D2 := Self.Configuration (2);
            D3 := Self.Configuration (3);

         when Legs.Left_Middle =>
            D1 := Self.Configuration (4);
            D2 := Self.Configuration (5);
            D3 := Self.Configuration (6);

         when Legs.Left_Hind =>
            D1 := Self.Configuration (7);
            D2 := Self.Configuration (8);
            D3 := Self.Configuration (9);

         when Legs.Right_Front =>
            D1 := Self.Configuration (10);
            D2 := Self.Configuration (11);
            D3 := Self.Configuration (12);

         when Legs.Right_Middle =>
            D1 := Self.Configuration (13);
            D2 := Self.Configuration (14);
            D3 := Self.Configuration (15);

         when Legs.Right_Hind =>
            D1 := Self.Configuration (16);
            D2 := Self.Configuration (17);
            D3 := Self.Configuration (18);
      end case;

      Kinematics.Set
        (Self    => Posture,
         Theta_1 => Kinematics.Theta_1 (Posture) + D1,
         Theta_2 => Kinematics.Theta_2 (Posture) + D2,
         Theta_3 => Kinematics.Theta_3 (Posture) + D3);
   end Update;

end Bodypath_Generators.Whole_Body;
