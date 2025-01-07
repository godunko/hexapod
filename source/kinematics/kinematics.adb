--
--  Copyright (C) 2023-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

package body Kinematics is

   ---------
   -- Set --
   ---------

   procedure Set
     (Self : out Position;
      X    : CGK.Reals.Real;
      Y    : CGK.Reals.Real;
      Z    : CGK.Reals.Real) is
   begin
      Self := CGK.Primitives.Points_3D.As_Point_3D (X => X, Y => Y, Z => Z);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self    : out Posture;
      Theta_1 : CGK.Reals.Real;
      Theta_2 : CGK.Reals.Real;
      Theta_3 : CGK.Reals.Real) is
   begin
      Self.Theta.M_1 := Theta_1;
      Self.Theta.M_2 := Theta_2;
      Self.Theta.M_3 := Theta_3;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self      : out Velocity;
      Linear_X  : CGK.Reals.Real;
      Linear_Y  : CGK.Reals.Real;
      Linear_Z  : CGK.Reals.Real;
      Angular_X : CGK.Reals.Real;
      Angular_Y : CGK.Reals.Real;
      Angular_Z : CGK.Reals.Real) is
   begin
      Self :=
        (Linear_X  => Linear_X,
         Linear_Y  => Linear_Y,
         Linear_Z  => Linear_Z,
         Angular_X => Angular_X,
         Angular_Y => Angular_Y,
         Angular_Z => Angular_Z);
   end Set;

   -------------------
   -- Set_Angular_Z --
   -------------------

   procedure Set_Angular_Z (Self : in out Velocity; To : CGK.Reals.Real) is
   begin
      Self.Angular_Z := To;
   end Set_Angular_Z;

end Kinematics;
