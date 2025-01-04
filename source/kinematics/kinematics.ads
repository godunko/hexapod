--
--  Copyright (C) 2023-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

private with CGK.Mathematics.Vectors_3;
with CGK.Reals;

package Kinematics
  with Pure
is

   type Position is private
     with Preelaborable_Initialization;

   function X (Self : Position) return CGK.Reals.Real with Inline;
   function Y (Self : Position) return CGK.Reals.Real with Inline;
   function Z (Self : Position) return CGK.Reals.Real with Inline;
   --  Coordinates of the position.

   procedure Set
     (Self : out Position;
      X    : CGK.Reals.Real;
      Y    : CGK.Reals.Real;
      Z    : CGK.Reals.Real) with Inline;

   type Orientation is record
      U : CGK.Reals.Real;
      V : CGK.Reals.Real;
      W : CGK.Reals.Real;
   end record;

   type Pose is record
      Position    : Kinematics.Position;
      Orientation : Kinematics.Orientation;
   end record;

   type Posture is private
     with Preelaborable_Initialization;
   --  Container of joint variables and a set of internal information.

   function Theta_1 (Self : Posture) return CGK.Reals.Real with Inline;
   function Theta_2 (Self : Posture) return CGK.Reals.Real with Inline;
   function Theta_3 (Self : Posture) return CGK.Reals.Real with Inline;
   --  Angular values of joint variables.

   procedure Set
     (Self    : out Posture;
      Theta_1 : CGK.Reals.Real;
      Theta_2 : CGK.Reals.Real;
      Theta_3 : CGK.Reals.Real);
   --  Set values of joint variables.

private

   type Position is record
      Value : CGK.Mathematics.Vectors_3.Vector_3;
   end record;

   function X (Self : Position) return CGK.Reals.Real is (Self.Value (0));
   function Y (Self : Position) return CGK.Reals.Real is (Self.Value (1));
   function Z (Self : Position) return CGK.Reals.Real is (Self.Value (2));

   type Posture is record
      Theta : CGK.Mathematics.Vectors_3.Vector_3;
      --  Vector of the joint variables.
   end record;

   function Theta_1 (Self : Posture) return CGK.Reals.Real is (Self.Theta (0));
   function Theta_2 (Self : Posture) return CGK.Reals.Real is (Self.Theta (1));
   function Theta_3 (Self : Posture) return CGK.Reals.Real is (Self.Theta (2));

end Kinematics;
