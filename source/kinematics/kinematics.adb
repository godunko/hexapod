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
      Self.Value (0) := X;
      Self.Value (1) := Y;
      Self.Value (2) := Z;
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
      Self.Theta (0) := Theta_1;
      Self.Theta (1) := Theta_2;
      Self.Theta (2) := Theta_3;
   end Set;

end Kinematics;
