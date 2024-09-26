--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Hexapod.Console;

package body Debug.Log.Console is

   L : aliased Logger;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Debug.Log.Logger := L'Access;
   end Initialize;

   ------------
   -- Output --
   ------------

   overriding procedure Output (Self : in out Logger; Item : String) is
   begin
      Hexapod.Console.Put (Item);
   end Output;

end Debug.Log.Console;
