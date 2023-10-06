--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;
with Ada.Text_IO;

with Kinematics.Forward;
with Kinematics.Inverse.Algebraic;
with Reals;

procedure Kinematics_Inverse_Algebraic is

   use Ada.Text_IO;
   use type Reals.Real;

   package Real_IO is new Ada.Text_IO.Float_IO (Reals.Real);
   use Real_IO;

   procedure Select_Posture
     (Current  : Kinematics.Posture;
      Solution : Kinematics.Inverse.Algebraic.Algebraic_Solution_Array;
      Selected : out Kinematics.Posture;
      Success  : out Boolean);

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

   --------------------
   -- Select_Posture --
   --------------------

   procedure Select_Posture
     (Current  : Kinematics.Posture;
      Solution : Kinematics.Inverse.Algebraic.Algebraic_Solution_Array;
      Selected : out Kinematics.Posture;
      Success  : out Boolean)
   is
      Out_Of_Range :
        array (Positive range 1 .. 4) of Boolean := (others => False);
      Distance     : Reals.Real;
      Min_Distance : Reals.Real := Reals.Real'Last;
      Min_Index    : Natural := 0;

   begin
      Success := False;

      for J in Solution'Range loop
         if Solution (J).Exists
           and then
             (Kinematics.Theta_1 (Solution (J).Posture)
                not in -Ada.Numerics.Pi / 2.0 .. Ada.Numerics.Pi / 2.0
              or Kinematics.Theta_2 (Solution (J).Posture)
                not in -Ada.Numerics.Pi / 2.0 .. Ada.Numerics.Pi / 2.0
              or Kinematics.Theta_3 (Solution (J).Posture)
                not in -Ada.Numerics.Pi / 2.0 .. Ada.Numerics.Pi)
                  --  XXX Left and right legs has different ranges of Theta_3
         then
            Out_Of_Range (J) := True;
         end if;
      end loop;

      for J in Solution'Range loop
         if Solution (J).Exists and not Out_Of_Range (J) then
            Distance :=
              Reals.Elementary_Functions.Sqrt
                ((Kinematics.Theta_1 (Solution (J).Posture)
                   - Kinematics.Theta_1 (Current)) ** 2
                 + (Kinematics.Theta_2 (Solution (J).Posture)
                   - Kinematics.Theta_1 (Current)) ** 2
                 + (Kinematics.Theta_3 (Solution (J).Posture)
                   - Kinematics.Theta_1 (Current)) ** 2);

            if Distance < Min_Distance then
               --  Select nearest position. Works good enough for now.

               Min_Distance := Distance;
               Min_Index    := J;
               Selected     := Solution (J).Posture;
               Success      := True;
            end if;
         end if;
      end loop;

      for J in Solution'Range loop
         if Solution (J).Exists then
            Put ("   ");
            Put (Solution (J).Posture);
            Put (": ");

            if Out_Of_Range (J) then
               Put ("OUT");

            elsif J /= Min_Index then
               Put ("FAR");

            else
               Put (Kinematics.Forward.RF_E_Position (Selected));
            end if;
         end if;
      end loop;
   end Select_Posture;

   P12 : constant := Ada.Numerics.Pi / 2.0;
   P14 : constant := Ada.Numerics.Pi / 4.0;
   P16 : constant := Ada.Numerics.Pi / 6.0;

   Current_Posture  : Kinematics.Posture;
   Current_Position : Kinematics.Position;
   Desired_Position : Kinematics.Position;
   Found_Posture    : Kinematics.Inverse.Algebraic.Algebraic_Solution_Array;
   Aux_Posture      : Kinematics.Posture;
   Solutions        : Natural;
   Success          : Boolean;

begin
   Put (Reals.Real'Epsilon);
   Put (Reals.Real'Model_Epsilon);
   New_Line;

   Kinematics.Set (Aux_Posture, 0.0, 0.0, 0.0);
   --  Kinematics.Set (Aux_Posture, P16, 0.0, P12);
   --  Kinematics.Set (Found_Posture, P16, -P14, 0.0);
   Desired_Position := Kinematics.Forward.RF_E_Position (Aux_Posture);

   --  Kinematics.Set (Desired_Position, 0.0741, 0.048, -0.050);
   --  Kinematics.Set (Desired_Position, 0.306, 0.048, 0.0);
   --  Kinematics.Set (Desired_Position, 0.187, 0.048, -0.124);

   Put (Desired_Position);
   Put (": ");
   Put (Aux_Posture);
   New_Line;

   Current_Posture  := Aux_Posture;
   Current_Position := Desired_Position;

   loop
      Put (Desired_Position);
      Put (": ");

      Kinematics.Inverse.Algebraic.RF_Solve
        (Desired_Position, Found_Posture, Solutions);

      if Solutions = 0 then
         Put ("no solution");

         exit;
      end if;

      Select_Posture (Current_Posture, Found_Posture, Aux_Posture, Success);

      exit when not Success;

      Current_Posture := Aux_Posture;
      Current_Position := Kinematics.Forward.RF_E_Position (Aux_Posture);

      Kinematics.Set
        (Desired_Position,
         Kinematics.X (Current_Position) - 0.010,
         Kinematics.Y (Current_Position) - 0.000,
         Kinematics.Z (Current_Position) - 0.000);

      New_Line;
   end loop;
end Kinematics_Inverse_Algebraic;
