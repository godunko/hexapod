--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with Reals;

package Kinematics is

   pragma Pure;

   type Position is private
     with Preelaborable_Initialization;

   function X (Self : Position) return Reals.Real with Inline;
   function Y (Self : Position) return Reals.Real with Inline;
   function Z (Self : Position) return Reals.Real with Inline;
   --  Coordinates of the position.

   procedure Set
     (Self : out Position;
      X    : Reals.Real;
      Y    : Reals.Real;
      Z    : Reals.Real) with Inline;

   type Orientation is record
      U : Reals.Real;
      V : Reals.Real;
      W : Reals.Real;
   end record;

   type Pose is record
      Position    : Kinematics.Position;
      Orientation : Kinematics.Orientation;
   end record;

   type Posture is private;
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

private

   type Position is record
      Value : Reals.Vectors_3.Vector_3;
   end record;

   function X (Self : Position) return Reals.Real is (Self.Value.M_1);
   function Y (Self : Position) return Reals.Real is (Self.Value.M_2);
   function Z (Self : Position) return Reals.Real is (Self.Value.M_3);

   type Posture is record
      Theta : Reals.Vectors_3.Vector_3;
      --  Vector of the joint variables.
   end record
     with Preelaborable_Initialization;

   function Theta_1 (Self : Posture) return Reals.Real is (Self.Theta.M_1);
   function Theta_2 (Self : Posture) return Reals.Real is (Self.Theta.M_2);
   function Theta_3 (Self : Posture) return Reals.Real is (Self.Theta.M_3);

end Kinematics;
