--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with System.Machine_Code;

with A0B.Callbacks.Generic_Non_Dispatching;
with A0B.Time;
with A0B.Types.GCC_Builtins;

package body Hexapod.Remote_Control.PS2C is

   Start_Delay_Duration         : constant A0B.Time.Duration := 0.000_020;
   Close_Delay_Duration         : constant A0B.Time.Duration := 0.000_010;
   Acknowledge_Timeout_Duration : constant A0B.Time.Duration := 0.000_100;
   --  Duration of the delay after chip select and start of the data transfer,
   --  completion of the last byte transfer and release of the chip select,
   --  timeout of the acknowledge signal.

   procedure Start_Exchange (Self : in out Communication_Driver'Class);

   procedure Initiate_Transfer (Self : in out Communication_Driver'Class);
   --  Initiate transfer of the next byte when ready (transfer of the current
   --  byte has been completed and acknowledge signal has been received).

   procedure On_Acknowledge (Self : in out Communication_Driver'Class);

   procedure On_Close_Delay (Self : in out Communication_Driver'Class);

   procedure On_Start_Delay (Self : in out Communication_Driver'Class);

   procedure On_Timeout (Self : in out Communication_Driver'Class);

   procedure On_Transfer_Done (Self : in out Communication_Driver'Class);

   package On_Acknowledge_Callbacks is
     new A0B.Callbacks.Generic_Non_Dispatching
           (Communication_Driver, On_Acknowledge);

   package On_Close_Delay_Callbacks is
     new A0B.Callbacks.Generic_Non_Dispatching
           (Communication_Driver, On_Close_Delay);

   package On_Start_Delay_Callbacks is
     new A0B.Callbacks.Generic_Non_Dispatching
           (Communication_Driver, On_Start_Delay);

   package On_Timeout_Callbacks is
     new A0B.Callbacks.Generic_Non_Dispatching
           (Communication_Driver, On_Timeout);

   package On_Transfer_Done_Callbacks is
     new A0B.Callbacks.Generic_Non_Dispatching
           (Communication_Driver, On_Transfer_Done);

   function Reverse_Bits
     (Item : A0B.Types.Unsigned_8) return A0B.Types.Unsigned_8;

   function Reverse_Bits
     (Item : A0B.Types.Unsigned_32) return A0B.Types.Unsigned_32;

   procedure Reverse_Bits
     (Source      :
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
      Destination : out
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Communication_Driver'Class) is
   begin
      Self.State := Initial;

      Self.Acknowledge.Set_Callback
        (On_Acknowledge_Callbacks.Create_Callback (Self));
   end Initialize;

   -----------------------
   -- Initiate_Transfer --
   -----------------------

   procedure Initiate_Transfer (Self : in out Communication_Driver'Class) is
      use type A0B.Types.Unsigned_32;

      Success : Boolean := True;

   begin
      if (Self.Index = A0B.Types.Unsigned_32'Last
            and Self.Select_Received and Self.Select_Acknowledged)
        or else (Self.Index /= A0B.Types.Unsigned_32'Last
                   and then (Self.Data_Received (Self.Index)
                               and Self.Data_Acknowledged (Self.Index)))
      then
         Self.Index := @ + 1;

         if Self.Index = 0 then
            Self.State := Command_Transfer_Await;

         elsif Self.Index = Self.Length then
            Self.State := Last_Transfer_Await;

         else
            Self.State := Data_Transfer_Await;
         end if;

         Self.SPI.Transfer
           (Transmit_Buffer   => Self.Buffer (Self.Index),
            Receive_Buffer    => Self.Buffer (Self.Index),
            Finished_Callback =>
              On_Transfer_Done_Callbacks.Create_Callback (Self),
            Success           => Success);

         if Self.State = Last_Transfer_Await then
            Self.Data_Acknowledged (Self.Index) := True;

         else
            A0B.Timer.Enqueue
              (Self.Timeout,
               On_Timeout_Callbacks.Create_Callback (Self),
               Acknowledge_Timeout_Duration);
         end if;
      end if;
   end Initiate_Transfer;

   --------------------
   -- On_Acknowledge --
   --------------------

   procedure On_Acknowledge (Self : in out Communication_Driver'Class) is
      use type A0B.Types.Unsigned_32;

   begin
      A0B.Timer.Cancel (Self.Timeout);

      case Self.State is
         when Select_Transfer_Await =>
            Self.Select_Acknowledged := True;
            Self.Initiate_Transfer;

         when Command_Transfer_Await =>
            pragma Assert (Self.Index = 0);

            Self.Data_Acknowledged (Self.Index) := True;
            Self.Initiate_Transfer;

         when Data_Transfer_Await =>
            Self.Data_Acknowledged (Self.Index) := True;
            Self.Initiate_Transfer;

         when others =>
            raise Program_Error;
      end case;
   end On_Acknowledge;

   --------------------
   -- On_Close_Delay --
   --------------------

   procedure On_Close_Delay (Self : in out Communication_Driver'Class) is
   begin
      Self.State   := Initial;
      Self.Failure := False;

      Self.Acknowledge.Disable_Interrupt;
      Self.SPI.Release_Device;

      Reverse_Bits (Self.Buffer, Self.Receive_Buffer.all);

      A0B.Callbacks.Emit (Self.On_Completed);
   end On_Close_Delay;

   --------------------
   -- On_Start_Delay --
   --------------------

   procedure On_Start_Delay (Self : in out Communication_Driver'Class) is
      Success : Boolean := True;

   begin
      Self.State := Select_Transfer_Await;

      Self.Acknowledge.Enable_Interrupt;

      Self.Select_Buffer := 2#1000_0000#;  --  16#01#
      Self.SPI.Transmit
        (Transmit_Buffer   => Self.Select_Buffer,
         Finished_Callback =>
           On_Transfer_Done_Callbacks.Create_Callback (Self),
         Success           => Success);

      A0B.Timer.Enqueue
        (Self.Timeout,
         On_Timeout_Callbacks.Create_Callback (Self),
         Acknowledge_Timeout_Duration);
      --  Setup operation timeout
   end On_Start_Delay;

   ----------------
   -- On_Timeout --
   ----------------

   procedure On_Timeout (Self : in out Communication_Driver'Class) is
   begin
      Self.State   := Initial;
      Self.Failure := True;

      Self.Acknowledge.Disable_Interrupt;
      Self.SPI.Release_Device;

      A0B.Callbacks.Emit (Self.On_Completed);
   end On_Timeout;

   ----------------------
   -- On_Transfer_Done --
   ----------------------

   procedure On_Transfer_Done (Self : in out Communication_Driver'Class) is
      use type A0B.Types.Unsigned_8;
      use type A0B.Types.Unsigned_32;

   begin
      case Self.State is
         when Select_Transfer_Await =>
            Self.Select_Received := True;

            --  if A0B.Types.Unsigned_8 (SPI0_Periph.RDR.RD) /= 16#FF# then
            --     --  Unexpected byte received.

            --     raise Program_Error;
            --  end if;

            Self.Initiate_Transfer;

         when Command_Transfer_Await =>
            Self.Data_Received (Self.Index) := True;
            Self.Length :=
              A0B.Types.Unsigned_32
                (Reverse_Bits (Self.Buffer (Self.Index)) and 16#0F#) * 2
                   + 2 - 1;
            Self.Initiate_Transfer;

         when Data_Transfer_Await =>
            Self.Data_Received (Self.Index) := True;
            Self.Initiate_Transfer;

         when Last_Transfer_Await =>
            Self.Data_Received (Self.Index) := True;
            Self.State := Close_Delay;
            A0B.Timer.Enqueue
              (Self.Close,
               On_Close_Delay_Callbacks.Create_Callback (Self),
               Close_Delay_Duration);

         when others =>
            raise Program_Error;
      end case;
   end On_Transfer_Done;

   ------------------
   -- Reverse_Bits --
   ------------------

   function Reverse_Bits
     (Item : A0B.Types.Unsigned_8) return A0B.Types.Unsigned_8
   is
      use type A0B.Types.Unsigned_32;

   begin
      return
         A0B.Types.Unsigned_8
           (A0B.Types.GCC_Builtins.bswap
              (Reverse_Bits (A0B.Types.Unsigned_32 (Item))));
   end Reverse_Bits;

   ------------------
   -- Reverse_Bits --
   ------------------

   function Reverse_Bits
     (Item : A0B.Types.Unsigned_32) return A0B.Types.Unsigned_32 is
   begin
      return Result : A0B.Types.Unsigned_32 do
         System.Machine_Code.Asm
           (Template => "rbit %0, %1",
            Outputs  => A0B.Types.Unsigned_32'Asm_Output ("=r", Result),
            Inputs   => A0B.Types.Unsigned_32'Asm_Input ("r", Item));
      end return;
   end Reverse_Bits;

   ------------------
   -- Reverse_Bits --
   ------------------

   procedure Reverse_Bits
     (Source      : A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
      Destination : out
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer)
   is
      type Unsigned_32_Buffer is array (0 .. 7) of A0B.Types.Unsigned_32
         with Alignment => 4, Size => 256;

      S : Unsigned_32_Buffer with Import, Address => Source'Address;
      D : Unsigned_32_Buffer with Import, Address => Destination'Address;

   begin
      D (0) := A0B.Types.GCC_Builtins.bswap (Reverse_Bits (S (0)));
      D (1) := A0B.Types.GCC_Builtins.bswap (Reverse_Bits (S (1)));
      D (2) := A0B.Types.GCC_Builtins.bswap (Reverse_Bits (S (2)));
      D (3) := A0B.Types.GCC_Builtins.bswap (Reverse_Bits (S (3)));
      D (4) := A0B.Types.GCC_Builtins.bswap (Reverse_Bits (S (4)));
      D (5) := A0B.Types.GCC_Builtins.bswap (Reverse_Bits (S (5)));
      D (6) := A0B.Types.GCC_Builtins.bswap (Reverse_Bits (S (6)));
      D (7) := A0B.Types.GCC_Builtins.bswap (Reverse_Bits (S (7)));
   end Reverse_Bits;

   --------------------
   -- Start_Exchange --
   --------------------

   procedure Start_Exchange (Self : in out Communication_Driver'Class) is
   begin
      pragma Assert (Self.State = Initial);

      Self.State := Start_Delay;

      --  Reset internal state

      Self.Select_Received     := False;
      Self.Select_Acknowledged := False;
      Self.Data_Received       := [others => False];
      Self.Data_Acknowledged   := [others => False];
      Self.Index               := A0B.Types.Unsigned_32'Last;

      --  Prepare data buffer

      Reverse_Bits (Self.Transmit_Buffer.all, Self.Buffer);

      --  Select SPI device

      Self.SPI.Select_Device;

      --  Enqueue start delay

      A0B.Timer.Enqueue
        (Self.Start,
         On_Start_Delay_Callbacks.Create_Callback (Self),
         Start_Delay_Duration);
   end Start_Exchange;

   --------------
   -- Transfer --
   --------------

   procedure Transfer
     (Self            : in out Communication_Driver'Class;
      Transmit_Buffer :
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
      Receive_Buffer  : out
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
      On_Completed    : A0B.Callbacks.Callback;
      Success         : in out Boolean) is
   begin
      if not Success or Self.State /= Initial then
         Success := False;

         return;
      end if;

      Self.Transmit_Buffer := Transmit_Buffer'Unrestricted_Access;
      Self.Receive_Buffer  := Receive_Buffer'Unrestricted_Access;
      Self.On_Completed    := On_Completed;

      Self.Start_Exchange;
   end Transfer;

end Hexapod.Remote_Control.PS2C;
