--
--  Copyright (C) 2023-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with CGK.Primitives.Points_3D;
with CGK.Reals;
with Reals;

package Kinematics is

   pragma Pure;

   subtype Position is CGK.Primitives.Points_3D.Point_3D;

   procedure Set
     (Self : out Position;
      X    : CGK.Reals.Real;
      Y    : CGK.Reals.Real;
      Z    : CGK.Reals.Real) with Inline;

   type Orientation is record
      U : Reals.Real;
      V : Reals.Real;
      W : Reals.Real;
   end record;

   type Pose is record
      Position    : Kinematics.Position;
      Orientation : Kinematics.Orientation;
   end record;

   type Posture is private
     with Preelaborable_Initialization;
   --  Container of joint variables and a set of internal information.

   function Theta_1 (Self : Posture) return Reals.Real with Inline;
   function Theta_2 (Self : Posture) return Reals.Real with Inline;
   function Theta_3 (Self : Posture) return Reals.Real with Inline;
   --  Angular values of joint variables.

   procedure Set
     (Self    : out Posture;
      Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real);
   --  Set values of joint variables.

   type Velocity is private with Preelaborable_Initialization;
   --  Velocity

   procedure Set
     (Self      : out Velocity;
      Linear_X  : CGK.Reals.Real;
      Linear_Y  : CGK.Reals.Real;
      Linear_Z  : CGK.Reals.Real;
      Angular_X : CGK.Reals.Real;
      Angular_Y : CGK.Reals.Real;
      Angular_Z : CGK.Reals.Real);

   function Linear_X (Self : Velocity) return CGK.Reals.Real;
   function Linear_Y (Self : Velocity) return CGK.Reals.Real;
   function Linear_Z (Self : Velocity) return CGK.Reals.Real;
   function Angular_X (Self : Velocity) return CGK.Reals.Real;
   function Angular_Y (Self : Velocity) return CGK.Reals.Real;
   function Angular_Z (Self : Velocity) return CGK.Reals.Real;

private

   type Posture is record
      Theta : Reals.Vectors_3.Vector_3;
      --  Vector of the joint variables.
   end record;

   function Theta_1 (Self : Posture) return Reals.Real is (Self.Theta.M_1);
   function Theta_2 (Self : Posture) return Reals.Real is (Self.Theta.M_2);
   function Theta_3 (Self : Posture) return Reals.Real is (Self.Theta.M_3);

   type Velocity is record
      Linear_X  : CGK.Reals.Real;
      Linear_Y  : CGK.Reals.Real;
      Linear_Z  : CGK.Reals.Real;
      Angular_X : CGK.Reals.Real;
      Angular_Y : CGK.Reals.Real;
      Angular_Z : CGK.Reals.Real;
   end record;

   function Linear_X
     (Self : Velocity) return CGK.Reals.Real is (Self.Linear_X);
   function Linear_Y
     (Self : Velocity) return CGK.Reals.Real is (Self.Linear_Y);
   function Linear_Z
     (Self : Velocity) return CGK.Reals.Real is (Self.Linear_Z);
   function Angular_X
     (Self : Velocity) return CGK.Reals.Real is (Self.Angular_X);
   function Angular_Y
     (Self : Velocity) return CGK.Reals.Real is (Self.Angular_Y);
   function Angular_Z
     (Self : Velocity) return CGK.Reals.Real is (Self.Angular_Z);

end Kinematics;
