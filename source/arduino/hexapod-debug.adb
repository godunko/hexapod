--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;

package body Hexapod.Debug is

   use type Reals.Real;

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

   ----------------------
   -- Parametric_Image --
   ----------------------

   function Parametric_Image
     (Item      : Reals.Real;
      Precision : Positive := 3) return String
   is
      P  : constant Integer := 10 ** Precision;
      I  : constant Integer := Integer (Item * Reals.Real (P));
      F  : constant Integer := I / P;
      A  : constant Integer := I mod P;
      FI : constant String := Integer'Image (F);
      AI : constant String := Integer'Image (A);
      B  : String (1 .. 3 + Precision) :=
        (1 => ' ', 2 => '0', 3 => '.', others => '0');

   begin
      B (2 - (FI'Length) + 1 .. 2) := FI (FI'First .. FI'Last);
      B (3 + Precision - (AI'Length - 1) + 1 .. 3 + Precision) :=
        AI (AI'First + 1 .. AI'Last);

      return B;
   end Parametric_Image;

   --------------------
   -- Position_Image --
   --------------------

   function Position_Image (Item : Kinematics.Position) return String is
   begin
      return
        "("
        & Coordinate_Image (Kinematics.X (Item))
        & " "
        & Coordinate_Image (Kinematics.Y (Item))
        & " "
        & Coordinate_Image (Kinematics.Z (Item))
        & ")";
   end Position_Image;

   -------------------
   -- Posture_Image --
   -------------------

   function Posture_Image (Item : Kinematics.Posture) return String is
   begin
      return
        "<"
        & Angle_Image (Kinematics.Theta_1 (Item))
        & " "
        & Angle_Image (Kinematics.Theta_2 (Item))
        & " "
        & Angle_Image (Kinematics.Theta_3 (Item))
        & ">";
   end Posture_Image;

end Hexapod.Debug;
