--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package Hexapod.Console is

   procedure Initialize;

   procedure Put (Item : String);

   procedure Put_Line (Item : String);

   procedure New_Line;

   procedure Get_Synchronous (Item : out Character);

   procedure Get_Asynchronous
     (Item    : out Character;
      Success : in out Boolean);

   procedure Flush_In_Failure_Mode;
   --  Flush content of the output buffers in failure mode. It is expected that
   --  interrupts are disabled by the caller.

private

   Logo : constant String :=
     ASCII.CR & ASCII.LF
     & ASCII.CR & ASCII.LF
     & "         /\ .---._" & ASCII.CR & ASCII.LF
     & "      /\/.-. /\ /\/\" & ASCII.CR & ASCII.LF
     & "     //\\oo //\\/\\\\" & ASCII.CR & ASCII.LF
     & "    //  /""/`---\\ \\""`-._" & ASCII.CR & ASCII.LF
     & "_.-'""           ""`-.`-." & ASCII.CR & ASCII.LF
     & ASCII.CR & ASCII.LF;

end Hexapod.Console;
