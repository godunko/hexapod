--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Algebraic solver of inserse kinematics.
--
--  Note, due to limitations of used symbolic mathematic toolboxes, this solver
--  assume fixed values of some parameters of the hexapod configuration:
--
--   - Base_Z     : 0.0
--   - DH_D1      : 0.0
--   - DH_D2      : 0.0
--   - DH_D3      : 0.0
--   - DH_Alpha_1 : <fixed> -Pi/2 (left legs) or -Pi/2 (right legs)
--   - DH_Alpha_2 : 0.0
--   - DH_Alpha_3 : 0.0

package Kinematics.Inverse.Algebraic is

   type Solution (Exists : Boolean := False) is record
      case Exists is
         when False =>
            null;

         when True =>
            Posture : Kinematics.Posture;
      end case;
   end record;

   type Algebraic_Solution_Array is array (1 .. 4) of Solution;

   procedure LF_Solve
     (Desired_Position : Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean);

   procedure RF_Solve
     (Position : Kinematics.Position;
      Solution : out Algebraic_Solution_Array;
      Found    : out Natural);

end Kinematics.Inverse.Algebraic;
