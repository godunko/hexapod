--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;
with Ada.Text_IO;

with Kinematics.Forward;
with Reals;

procedure Trajectory is

   use Ada.Text_IO;
   use type Reals.Real;

   package Real_IO is new Ada.Text_IO.Float_IO (Reals.Real);
   use Real_IO;

   procedure Swing
     (T_sw : Reals.Real;  --  time
      S    : Reals.Real;  --  step length
      T    : Reals.Real;
      X    : out Reals.Real;
      Y    : out Reals.Real;
      Z    : out Reals.Real);

   -----------
   -- Swing --
   -----------

   procedure Swing
     (T_sw : Reals.Real;
      S    : Reals.Real;
      T    : Reals.Real;
      X    : out Reals.Real;
      Y    : out Reals.Real;
      Z    : out Reals.Real)
   is
      Aux      : Reals.Real;
      Negative : Boolean;

   begin
      X :=
        T / T_sw
          - 1.0 / (2.0 * Ada.Numerics.Pi)
            * Reals.Elementary_Functions.Sin
        (2.0 * Ada.Numerics.Pi * T / T_sw);
      Y := X;

      Aux :=
        2.0 * (T / T_sw - Reals.Elementary_Functions.Sin (4.0 *Ada.Numerics.Pi*t)/(4.0*Ada.Numerics.Pi)) - 1.0;
        --  2.0
        --   * (T / T_sw - Reals.Elementary_Functions.Sin
        --                   (4.0 * Ada.Numerics.Pi * T / T_sw)
        --                      / (4.0 * Ada.Numerics.Pi)
        --   - 1.0);

      Negative := (if T_sw / 2.0 - T < 0.0 then True else False);

      --  if Negative xor Aux < 0.0 then
      if Negative then
         Z := -Aux + 1.0;

      else
         Z := Aux + 1.0;
      end if;
   end Swing;

   T_sw : constant := 1.0;
   T_st : constant := 1.0;
   Step_Length : constant := 1.0;
   Frac : constant := 50;

   X : Reals.Real;
   Y : Reals.Real;
   Z : Reals.Real;
   Position : Kinematics.Position;
   Posture  : Kinematics.Posture;

begin
   for J in 0 .. Frac loop
      Swing
        (T_sw, Step_Length, T_sw / Reals.Real (Frac) * Reals.Real (J), X, Y, Z);
      Put (X, Exp => 0);
      Put (Y, Exp => 0);
      Put (Z, Exp => 0);
      New_Line;
   end loop;
end Trajectory;
