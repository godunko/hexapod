--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Integer_Text_IO;
with Ada.Numerics;
with Ada.Text_IO;

with Kinematics.Forward;
with Kinematics.Inverse.Levenberg_Marquardt;
with Reals;

procedure Kinematics_Inverse_Levenberg_Marquardt is

   use Ada.Integer_Text_IO;
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

   Current_Posture   : Kinematics.Posture;
   Current_Position  : Kinematics.Position;
   Desired_Position  : Kinematics.Position;
   Found_Posture     : Kinematics.Posture;
   Found_Position    : Kinematics.Position;
   Success           : Boolean;
   Count             : Natural;

begin
   Kinematics.Set (Current_Posture, 0.0, 0.0, 0.0);
   Current_Position := Kinematics.Forward.LF_E_Position (Current_Posture);

   Kinematics.Set (Desired_Position, 0.300, 0.048, 0.0);

   loop
      Kinematics.Inverse.Levenberg_Marquardt.Solve
        (Current_Position => Current_Position,
         Current_Posture  => Current_Posture,
         Desired_Position => Desired_Position,
         Found_Position   => Found_Position,
         Found_Posture    => Found_Posture,
         Success          => Success,
         Count            => Count);

      Put (Current_Position);
      Put (" to ");
      Put (Desired_Position);
      Put (": ");

      if Success then
         Put (Found_Posture);
         Put (Found_Position);
         Put (" (");
         Put (Count);
         Put (" steps)");
         New_Line;

      else
         Put ("FAILURE");
         Put (" (");
         Put (Count);
         Put (" steps)");
         New_Line;

         exit;
      end if;

      Current_Position := Found_Position;
      Current_Posture  := Found_Posture;
      Kinematics.Set
        (Desired_Position,
         Kinematics.X (Desired_Position) - 0.010,
         Kinematics.Y (Desired_Position),
         Kinematics.Z (Desired_Position));
   end loop;
end Kinematics_Inverse_Levenberg_Marquardt;
