--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Hexapod.Console is

   pragma Preelaborate;

   procedure Initialize;

   procedure Put (Item : String);

   procedure Put_Line (Item : String);

   procedure New_Line;

   procedure Get_Synchronous (Item : out Character);

   procedure Get_Asynchronous
     (Item    : out Character;
      Success : in out Boolean);

end Hexapod.Console;
