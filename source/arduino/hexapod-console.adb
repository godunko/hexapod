--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces;
with System.Storage_Elements;

with BBF.Board.UART;

with BBF.HPL.NVIC;
with BBF.HPL.PIO;
with BBF.HPL.PMC;
with BBF.HPL.UART;

with BBF.HRI.UART;

package body Hexapod.Console is

   UART_RX : constant := 8;
   UART_TX : constant := 9;

   procedure UART_Handler
     with Export,
          Convention    => C,
          External_Name => "UART_Handler";
   --  UART interrupt handler

   Logo : String :=
     ASCII.CR & ASCII.LF
     & "          /\ .---._" & ASCII.CR & ASCII.LF
     & "       /\/.-. /\ /\/\" & ASCII.CR & ASCII.LF
     & "     //\\oo //\\/\\\\" & ASCII.CR & ASCII.LF
     & "    //  /""/`---\\ \\""`-._" & ASCII.CR & ASCII.LF
     & "_.-'""           ""`-.`-." & ASCII.CR & ASCII.LF
     & ASCII.CR & ASCII.LF;

   Buffer : String (1 .. 256) := (others => '.');
   --  Internal I/O buffer.

   First  : Positive := 1;
   Length : Natural  := 0;
   --  Index of the first element need to be transmitted and number of
   --  elements.

   Transmission_Active : Boolean := False;
   --  Status of the data transmission. True means that data transmission
   --  is in progress, and transmission of the next portion of the data will
   --  be initiated by the interrupt handler. False means that Append should
   --  initiate data transmission.

   Receive_Buffer : Interfaces.Unsigned_8 with Volatile;
   Received       : Boolean := False with Volatile;

   procedure Initiate_Transmission
     with Pre => not Transmission_Active;

   ------------
   -- Append --
   ------------

   procedure Append (Data : String) is
      From    : Positive;
      Chunk_1 : Positive;
      Chunk_2 : Natural;

   begin
      if Data'Length not in 1 .. Buffer'Length then
         return;
      end if;

      --  Disable interrupts to protect critical section.

      BBF.HPL.Disable_Interrupts;

      --  Copy data into the buffer.

      From := First + Length;
      From := (if @ > Buffer'Last then @ - Buffer'Last else @);

      Chunk_1 := Integer'Min (Buffer'Last - From + 1, Data'Length);
      Chunk_2 := (if Chunk_1 < Data'Length then Data'Length - Chunk_1 else 0);

      Buffer (From .. From + Chunk_1 - 1) :=
        Data (Data'First .. Data'First + Chunk_1 - 1);
      Buffer (Buffer'First .. Buffer'First + Chunk_2 - 1) :=
        Data (Data'First + Chunk_1 .. Data'Last);
      Length := @ + Data'Length;
      Length := Integer'Min (Buffer'Length, Length);

      --  Initiate transmission when there is no transmission in progress.

      if not Transmission_Active then
         Initiate_Transmission;
      end if;

      --  Enable interrupts.

      BBF.HPL.Enable_Interrupts;
   end Append;

   ----------------------
   -- Get_Asynchronous --
   ----------------------

   procedure Get_Asynchronous
     (Item    : out Character;
      Success : in out Boolean) is
   begin
      if not Success then
         Item := Character'Val (0);

         return;
      end if;

      if not Received then
         Success := False;
         Item    := Character'Val (0);

      else
         Success  := True;
         Item     := Character'Val (Receive_Buffer);
         Received := False;
      end if;
   end Get_Asynchronous;

   ---------------------
   -- Get_Synchronous --
   ---------------------

   procedure Get_Synchronous (Item : out Character) is
   begin
      while not Received loop
         null;
      end loop;

      Item := Character'Val (Receive_Buffer);
      Received := False;
   end Get_Synchronous;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      BBF.Board.UART.UART.Initialize;

      Put (Logo);
      --  Send logo
   end Initialize;

   ---------------------------
   -- Initiate_Transmission --
   ---------------------------

   procedure Initiate_Transmission is
      Chunk_1 : Positive;
      Chunk_2 : Natural;

   begin
      if Length = 0 then
         return;
      end if;

      Chunk_1 := Integer'Min (Buffer'Last - First + 1, Length);
      Chunk_2 := (if Chunk_1 < Length then Length - Chunk_1 else 0);

      --  Initiate PDC transmission.

      if Chunk_2 = 0 then
         BBF.HPL.UART.Set_Transmission_Buffer
           (BBF.HPL.UART.UART0,
            Buffer (First)'Address,
            Interfaces.Unsigned_16 (Chunk_1));
         First := @ + Chunk_1;
         First := (if @ > Buffer'Last then @ - Buffer'Length else @);

      else
         BBF.HPL.UART.Set_Transmission_Buffer
           (BBF.HPL.UART.UART0,
            Buffer (First)'Address,
            Interfaces.Unsigned_16 (Chunk_1),
            Buffer (Buffer'First)'Address,
            Interfaces.Unsigned_16 (Chunk_2));
         First := Buffer'First + Chunk_2;
      end if;

      Length := 0;

      Transmission_Active := True;
      BBF.HPL.UART.Enable_Interrupt
        (BBF.HPL.UART.UART0, BBF.HPL.UART.Transmission_Buffer_Empty);
   end Initiate_Transmission;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Append (ASCII.CR & ASCII.LF);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Item : String) is
   begin
      Append (Item);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Item : String) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;

   ------------------
   -- UART_Handler --
   ------------------

   S : Boolean := False;

   procedure UART_Handler is
      Success : Boolean;

   begin
      S := not S;
      BBF.Board.Pin_13_LED.Set (S);

      BBF.HPL.Disable_Interrupts;

      if BBF.HPL.UART.Is_Receiver_Ready (BBF.HPL.UART.UART0) then
         BBF.HPL.UART.Read (BBF.HPL.UART.UART0, Receive_Buffer, Success);
         Received := True;
      end if;

      if BBF.HPL.UART.Is_Transmission_Buffer_Empty (BBF.HPL.UART.UART0) then
         BBF.HPL.UART.Disable_Interrupt
           (BBF.HPL.UART.UART0, BBF.HPL.UART.Transmission_Buffer_Empty);
         Transmission_Active := False;

         Initiate_Transmission;
      end if;

      --  Disable interrupts at all

      BBF.HPL.Enable_Interrupts;
   end UART_Handler;

end Hexapod.Console;
