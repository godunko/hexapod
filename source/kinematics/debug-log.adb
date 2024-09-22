--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with Debug.Log.Loggers;

package body Debug.Log is

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      if Logger /= null then
         Logger.Output (ASCII.CR & ASCII.LF);
      end if;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Item : String) is
   begin
      if Logger /= null then
         Logger.Output (Item);
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Item : String) is
   begin
      if Logger /= null then
         Logger.Output (Item);
         New_Line;
      end if;
   end Put_Line;

end Debug.Log;
