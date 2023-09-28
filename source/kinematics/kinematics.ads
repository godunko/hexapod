--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with Reals;

package Kinematics is

   pragma Pure;

   type Position is record
      X : Reals.Real;
      Y : Reals.Real;
      Z : Reals.Real;
   end record;

   type Orientation is record
      U : Reals.Real;
      V : Reals.Real;
      W : Reals.Real;
   end record;

   type Pose is record
      Position    : Kinematics.Position;
      Orientation : Kinematics.Orientation;
   end record;

   type Posture is record
      Theta_1 : Reals.Real;
      Theta_2 : Reals.Real;
      Theta_3 : Reals.Real;
   end record;

end Kinematics;
