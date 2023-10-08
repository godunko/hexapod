--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;

with Hexapod.COnsole;
with Kinematics.Forward;
with Kinematics.Inverse.Algebraic;

package body Trajectory is

   use type Reals.Real;

   T_Sw        : constant := 1.0;
   Step_Height : constant := 0.020;

   function Swing_XY (T : Reals.Real) return Reals.Real;
   function Swing_Z (T : Reals.Real) return Reals.Real;

   procedure Select_Posture
     (Current  : Kinematics.Posture;
      Solution : Kinematics.Inverse.Algebraic.Algebraic_Solution_Array;
      Selected : out Kinematics.Posture;
      Success  : out Boolean);

   -----------------
   -- Angle_Image --
   -----------------

   function Angle_Image (Item : Reals.Real) return String is
      I  : constant Integer := Integer (Item / Ada.Numerics.Pi * 180_000.0);
      F  : constant Integer := I / 1_000;
      A  : constant Integer := I mod 1_000;
      FI : constant String := Integer'Image (F);
      AI : constant String := Integer'Image (A);
      B  : String := "   0.000";

   begin
      B (4 - (FI'Length) + 1 .. 4) := FI (FI'First .. FI'Last);
      B (8 - (AI'Length - 1) + 1 .. 8) := AI (AI'First + 1 .. AI'Last);

      return B;
   end Angle_Image;

   ----------------------
   -- Coordinate_Image --
   ----------------------

   function Coordinate_Image (Item : Reals.Real) return String is
      I  : constant Integer := Integer (Item * 1_000_000.0);
      F  : constant Integer := I / 1_000;
      A  : constant Integer := I mod 1_000;
      FI : constant String := Integer'Image (F);
      AI : constant String := Integer'Image (A);
      B  : String := "   0.000";

   begin
      B (4 - (FI'Length) + 1 .. 4) := FI (FI'First .. FI'Last);
      B (8 - (AI'Length - 1) + 1 .. 8) := AI (AI'First + 1 .. AI'Last);

      return B;
   end Coordinate_Image;

   ------------------
   -- Move_Support --
   ------------------

   procedure Move_Support
     (From        : Kinematics.Position;
      To          : Kinematics.Position;
      Current     : Kinematics.Posture;
      T           : Reals.Real;
      New_Posture : out Kinematics.Posture)
   is
      X : constant Reals.Real :=
        Kinematics.X (From)
          + (Kinematics.X (To) - Kinematics.X (From)) * T;
      Y : constant Reals.Real :=
        Kinematics.Y (From)
          + (Kinematics.Y (To) - Kinematics.Y (From)) * T;
      Z : constant Reals.Real :=
        Kinematics.Z (From)
          + (Kinematics.Z (To) - Kinematics.Z (From)) * T;
      P : Kinematics.Position;
      S : Kinematics.Inverse.Algebraic.Algebraic_Solution_Array;
      N : Natural;
      B : Boolean;

   begin
      Kinematics.Set (P, X, Y, Z);
      --  Hexapod.Console.Put
      --    (Coordinate_Image (Kinematics.X (P))
      --     & Coordinate_Image (Kinematics.Y (P))
      --     & Coordinate_Image (Kinematics.Z (P)));
      Kinematics.Inverse.Algebraic.RF_Solve (P, S, N);
      Select_Posture (Current, S, New_Posture, B);

      if not B then
         raise Program_Error;
      end if;
   end Move_Support;

   ----------------
   -- Move_Swing --
   ----------------

   procedure Move_Swing
     (From        : Kinematics.Position;
      To          : Kinematics.Position;
      Current     : Kinematics.Posture;
      T           : Reals.Real;
      New_Posture : out Kinematics.Posture)
   is
      X : constant Reals.Real :=
        Kinematics.X (From)
          + (Kinematics.X (To) - Kinematics.X (From)) * Swing_XY (T);
      Y : constant Reals.Real :=
        Kinematics.Y (From)
          + (Kinematics.Y (To) - Kinematics.Y (From)) * Swing_XY (T);

      Z : constant Reals.Real :=
        Kinematics.Z (From)
          + (Kinematics.Z (To) - Kinematics.Z (From)) * T
          + Step_Height * Swing_Z (T);

      S : Kinematics.Inverse.Algebraic.Algebraic_Solution_Array;
      N : Natural;
      B : Boolean;

      P : Kinematics.Position;

   begin
      --  Hexapod.Console.Put ("FROM: ");
      --  Hexapod.Console.Put
      --    (Coordinate_Image (Kinematics.X (From))
      --     & Coordinate_Image (Kinematics.Y (From))
      --     & Coordinate_Image (Kinematics.Z (From)));
      --  Hexapod.Console.Put (" TO: ");
      --  Hexapod.Console.Put
      --    (Coordinate_Image (Kinematics.X (To))
      --     & Coordinate_Image (Kinematics.Y (To))
      --     & Coordinate_Image (Kinematics.Z (To)));
      --  Hexapod.Console.Put
      --    (" T:" & Reals.Real'Image (T));
      --  Hexapod.Console.New_Line;
      --  Hexapod.Console.Put
      --    (" TXY:"
      --     & Reals.Real'Image (Swing_XY (T))
      --     & " TZ:"
      --     & Reals.Real'Image (Swing_Z (T))
      --    );

      Kinematics.Set (P, X, Y, Z);
      --  Hexapod.Console.Put
      --    (Coordinate_Image (Kinematics.X (P))
      --     & Coordinate_Image (Kinematics.Y (P))
      --     & Coordinate_Image (Kinematics.Z (P)));
      Kinematics.Inverse.Algebraic.RF_Solve (P, S, N);
      Select_Posture (Current, S, New_Posture, B);

      if not B then
         raise Program_Error;
      end if;
   end Move_Swing;

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

      --  for J in Solution'Range loop
      --     if Solution (J).Exists then
      --        Hexapod.Console.Put ("   ");
      --        Hexapod.Console.Put
      --          (Angle_Image (Kinematics.Theta_1 (Solution (J).Posture))
      --           & Angle_Image (Kinematics.Theta_2 (Solution (J).Posture))
      --           & Angle_Image (Kinematics.Theta_3 (Solution (J).Posture)));
      --        Hexapod.Console.Put (": ");
      --
      --        if Out_Of_Range (J) then
      --           Hexapod.Console.Put ("OUT");
      --
      --        elsif J /= Min_Index then
      --           Hexapod.Console.Put ("FAR");
      --
      --        else
      --           Hexapod.Console.Put
      --             (Coordinate_Image
      --                (Kinematics.X (Kinematics.Forward.RF_E_Position (Selected)))
      --              & Coordinate_Image
      --                (Kinematics.X (Kinematics.Forward.RF_E_Position (Selected)))
      --              & Coordinate_Image
      --                (Kinematics.X (Kinematics.Forward.RF_E_Position (Selected))));
      --        end if;
      --
      --     else
      --        Hexapod.Console.Put_Line ("N/A");
      --     end if;
      --  end loop;
   end Select_Posture;

   --------------
   -- Swing_XY --
   --------------

   function Swing_XY (T : Reals.Real) return Reals.Real is
   begin
      return
        T / T_sw
          - 1.0 / (2.0 * Ada.Numerics.Pi)
            * Reals.Elementary_Functions.Sin
               (2.0 * Ada.Numerics.Pi * T / T_sw);
   end Swing_XY;

   -------------
   -- Swing_Z --
   -------------

   function Swing_Z (T : Reals.Real) return Reals.Real is
      Aux      : Reals.Real;
      Negative : Boolean;

   begin
      --  Y := X;

      Aux :=
        2.0 * (T / T_sw
                 - Reals.Elementary_Functions.Sin
                    (4.0 * Ada.Numerics.Pi * T / T_sw)
                       / (4.0 * Ada.Numerics.Pi)) - 1.0;

      Negative := (if T_sw / 2.0 - T < 0.0 then True else False);

      if Negative then
         return -Aux + 1.0;

      else
         return Aux + 1.0;
      end if;
   end Swing_Z;

end Trajectory;
