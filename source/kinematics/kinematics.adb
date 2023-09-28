--
--  Copyright (C) 2023, Vadim Godunko
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
      X    : Reals.Real;
      Y    : Reals.Real;
      Z    : Reals.Real) is
   begin
      Self.Value.M_1 := X;
      Self.Value.M_2 := Y;
      Self.Value.M_3 := Z;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self    : out Posture;
      Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real) is
   begin
      Self.Theta.M_1 := Theta_1;
      Self.Theta.M_2 := Theta_2;
      Self.Theta.M_3 := Theta_3;
   end Set;

end Kinematics;
