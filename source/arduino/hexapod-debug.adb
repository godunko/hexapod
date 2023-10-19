--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Hexapod.Debug is

   ----------------------
   -- Coordinate_Image --
   ----------------------

   function Coordinate_Image (Item : Reals.Real) return String is
      use type Reals.Real;

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

end Hexapod.Debug;
