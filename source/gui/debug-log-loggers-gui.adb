--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO;

package body Debug.Log.Loggers.GUI is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Logger := new GUI_Logger;
   end Initialize;

   ------------
   -- Output --
   ------------

   overriding procedure Output (Self : in out GUI_Logger; Item : String) is
   begin
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Item);
      Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
   end Output;

end Debug.Log.Loggers.GUI;
