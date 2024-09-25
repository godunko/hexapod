--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces;

with BBF.Board.UART;

package body Hexapod.Console is

   Logo : constant String :=
     ASCII.CR & ASCII.LF
     & "          /\ .---._" & ASCII.CR & ASCII.LF
     & "       /\/.-. /\ /\/\" & ASCII.CR & ASCII.LF
     & "     //\\oo //\\/\\\\" & ASCII.CR & ASCII.LF
     & "    //  /""/`---\\ \\""`-._" & ASCII.CR & ASCII.LF
     & "_.-'""           ""`-.`-." & ASCII.CR & ASCII.LF
     & ASCII.CR & ASCII.LF;

   UART : BBF.Board.UART.UART_Driver
            (Receive_Queue => 0, Transmit_Queue => 255);

   ----------------------
   -- Get_Asynchronous --
   ----------------------

   procedure Get_Asynchronous
     (Item    : out Character;
      Success : in out Boolean)
   is
      use type BBF.Unsigned_16;

      Buffer : BBF.Unsigned_8_Array_16 (0 ..0);
      Size   : BBF.Unsigned_16;

   begin
      if not Success then
         Item := Character'Val (0);

         return;
      end if;

      UART.Receive_Asynchronous (Buffer, Size);

      if Size = 0 then
         Success := False;
         Item    := Character'Val (0);

      else
         Success := True;
         Item    := Character'Val (Buffer (0));
         --  Received := False;
      end if;
   end Get_Asynchronous;

   ---------------------
   -- Get_Synchronous --
   ---------------------

   procedure Get_Synchronous (Item : out Character) is
      use type BBF.Unsigned_16;

      Buffer : BBF.Unsigned_8_Array_16 (0 ..0);
      Size   : BBF.Unsigned_16;

   begin
      loop
         UART.Receive_Asynchronous (Buffer, Size);

         if Size /= 0 then
            Item := Character'Val (Buffer (0));

            return;
         end if;
      end loop;
   end Get_Synchronous;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      UART.Initialize;

      Put (Logo);
      --  Send logo
   end Initialize;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Put (ASCII.CR & ASCII.LF);
   end New_Line;

   ----------------------
   -- On_Failure_Flush --
   ----------------------

   procedure On_Failure_Flush is
   begin
      null;
   end On_Failure_Flush;

   ---------
   -- Put --
   ---------

   procedure Put (Item : String) is
      use type BBF.Unsigned_16;

      Data : constant BBF.Byte_Array_16 (0 .. Item'Length - 1)
        with Import, Address => Item (Item'First)'Address;

   begin
      UART.Transmit_Asynchronous (Data);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Item : String) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;

end Hexapod.Console;
