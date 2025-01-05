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

end Kinematics;
