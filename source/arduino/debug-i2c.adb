--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with Interfaces;

with Hexapod.Console;

package body Debug.I2C is

   ----------
   -- Dump --
   ----------

   procedure Dump (Data : BBF.I2C.Unsigned_8_Array) is

      use type Interfaces.Unsigned_8;

      N2H  : constant array (Interfaces.Unsigned_8 range 0 .. 15) of Character :=
        "0123456789ABCDEF";
      Line : String (1 .. Data'Length * 3) := (others => ' ');

   begin
      for J in Data'Range loop
         Line ((J - Data'First) * 3 + 2 .. (J - Data'First) * 3 + 3) :=
           (1 => N2H (Data (J) / 16),
            2 => N2H (Data (J) mod 16));
      end loop;

      Hexapod.Console.Put_Line (Line);
   end Dump;

end Debug.I2C;
