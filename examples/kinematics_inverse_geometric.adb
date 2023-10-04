--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;
with Ada.Text_IO;

with Kinematics.Forward;
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
      Put (Kinematics.Theta_1 (Item) / Ada.Numerics.Pi * 180.0, Fore => 4, Aft => 3, Exp => 0);
      Put (", ");
      Put (Kinematics.Theta_2 (Item) / Ada.Numerics.Pi * 180.0, Fore => 4, Aft => 3, Exp => 0);
      Put (", ");
      Put (Kinematics.Theta_3 (Item) / Ada.Numerics.Pi * 180.0, Fore => 4, Aft => 3, Exp => 0);
      Put (")");
   end Put;

   P12 : constant := Ada.Numerics.Pi / 2.0;
   P14 : constant := Ada.Numerics.Pi / 4.0;
   P16 : constant := Ada.Numerics.Pi / 6.0;

   Desired_Position  : Kinematics.Position;
   Found_Posture     : Kinematics.Posture;
   Success           : Boolean;
   Position          : Kinematics.Position;

begin
   Put (Reals.Real'Epsilon);
   Put (Reals.Real'Model_Epsilon);
   New_Line;

   --  Kinematics.Set (Found_Posture, 0.0, 0.0, 0.0);
   Kinematics.Set (Found_Posture, P16, 0.0, P12);
   --  Kinematics.Set (Found_Posture, P16, -P14, 0.0);
   Desired_Position := Kinematics.Forward.RF_E_Position (Found_Posture);

   Put (Desired_Position);
   Put (": ");
   Put (Found_Posture);
   New_Line;

   --  Kinematics.Set (Desired_Position, 0.0741, 0.048, -0.050);
   --  Kinematics.Set (Desired_Position, 0.306, 0.048, 0.0);
   --  Kinematics.Set (Desired_Position, 0.187, 0.048, -0.124);

   loop
      Put (Desired_Position);
      Put (": ");

      Kinematics.Inverse.Geometric.RF_Solve
        (Desired_Position,
         Found_Posture,
         Success);

      if Success then
         Put (Found_Posture);
         Position := Kinematics.Forward.RF_E_Position (Found_Posture);
         Put (Position);

      else
         Put ("no solution");

         exit;
      end if;

      Kinematics.Set
        (Desired_Position,
         Kinematics.X (Position) - 0.010,
         Kinematics.Y (Position) - 0.000,
         Kinematics.Z (Position) - 0.000);

      New_Line;
   end loop;
end Kinematics_Inverse_Geometric;
