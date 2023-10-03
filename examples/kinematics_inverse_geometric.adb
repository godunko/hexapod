--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;
with Ada.Text_IO;

with Kinematics.Inverse.Geometric;
with Reals;

procedure Kinematics_Inverse_Geometric is

   use Ada.Text_IO;
   use type Reals.Real;

   package Real_IO is new Ada.Text_IO.Float_IO (Reals.Real);
   use Real_IO;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Kinematics.Position) is
   begin
      Put ("(");
      Put (Kinematics.X (Item), Fore => 2, Aft => 6, Exp => 0);
      Put (", ");
      Put (Kinematics.Y (Item), Fore => 2, Aft => 6, Exp => 0);
      Put (", ");
      Put (Kinematics.Z (Item), Fore => 2, Aft => 6, Exp => 0);
      Put (")");
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Kinematics.Posture) is
   begin
      Put ("(");
      Put (Kinematics.Theta_1 (Item) / Ada.Numerics.Pi * 180.0, Fore => 3, Aft => 3, Exp => 0);
      Put (", ");
      Put (Kinematics.Theta_2 (Item) / Ada.Numerics.Pi * 180.0, Fore => 3, Aft => 3, Exp => 0);
      Put (", ");
      Put (Kinematics.Theta_3 (Item) / Ada.Numerics.Pi * 180.0, Fore => 3, Aft => 3, Exp => 0);
      Put (")");
   end Put;

   Desired_Position  : Kinematics.Position;
   Found_Posture     : Kinematics.Posture;
   Success           : Boolean;

begin
   Put (Reals.Real'Epsilon);
   Put (Reals.Real'Model_Epsilon);
   New_Line;

   Kinematics.Set (Desired_Position, 0.0741, 0.048, -0.050);
   --  Kinematics.Set (Desired_Position, 0.306, 0.048, 0.0);
   --  Kinematics.Set (Desired_Position, 0.187, 0.048, -0.124);

   Put (Desired_Position);
   Put (": ");

   Kinematics.Inverse.Geometric.Solve
     (Desired_Position,
      Found_Posture,
      Success);

   if Success then
      Put (Found_Posture);

   else
      Put ("no solution");
   end if;

   New_Line;
end Kinematics_Inverse_Geometric;
