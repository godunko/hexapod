--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Blackpill STM32F401 version
--
--  This version uses interrupts and DMA for data transfer in normal operation
--  mode. However, it allows to complete current operation and to flush output
--  buffers when system is blocked due to failure. In last case it is assumed
--  that interrupts are disabled outside of this package.
--
--  Used peripherals: USART6, DMA2 Stream6 for data transmission, PA11 & PA12.

with Ada.Synchronous_Task_Control;
with Interfaces;

with A0B.ARMv7M.NVIC_Utilities;
with A0B.STM32F401.DMA.DMA2.Stream6;
with A0B.STM32F401.GPIO.PIOA;
with A0B.STM32F401.SVD.RCC;   use A0B.STM32F401.SVD.RCC;
with A0B.STM32F401.SVD.USART; use A0B.STM32F401.SVD.USART;
with A0B.STM32F401.USART_Function_Lines;
with A0B.Types;

with A0B.Callbacks.Generic_Parameterless;

package body Hexapod.Console is

   use type A0B.Types.Unsigned_32;

   TX_Line : A0B.STM32F401.GPIO.GPIO_Line renames A0B.STM32F401.GPIO.PIOA.PA11;
   RX_Line : A0B.STM32F401.GPIO.GPIO_Line renames A0B.STM32F401.GPIO.PIOA.PA12;

   type Unsigned_8_Array is
     array (A0B.Types.Unsigned_32 range <>) of A0B.Types.Unsigned_8;

   procedure USART6_Handler
     with Export, Convention => C, External_Name => "USART6_Handler";

   procedure DMA2_Stream6_Handler;
     --  with Export, Convention => C, External_Name => "DMA2_Stream6_Handler";

   Transmit_Buffer : Unsigned_8_Array (0 .. 511);
   Transmit_Head   : Interfaces.Unsigned_32 := 0;
   Transmit_Tail   : Interfaces.Unsigned_32 := 0;
   Receive_Buffer  : Unsigned_8_Array (0 .. 31);
   Receive_Head    : Interfaces.Unsigned_32 := 0;
   Receive_Tail    : Interfaces.Unsigned_32 := 0;

   Lock            : Ada.Synchronous_Task_Control.Suspension_Object;

   package On_DMA_Interrupt_Callbacks is
     new A0B.Callbacks.Generic_Parameterless (DMA2_Stream6_Handler);

   Transmit_Stream : A0B.STM32F401.DMA.DMA_Stream
     renames A0B.STM32F401.DMA.DMA2.Stream6.DMA2_Stream6;

   --------------------------
   -- DMA2_Stream6_Handler --
   --------------------------

   procedure DMA2_Stream6_Handler is
   begin
      if Transmit_Stream.Get_Masked_And_Clear_Transfer_Completed then
         Transmit_Stream.Disable;

      else
         raise Program_Error;
      end if;
   end DMA2_Stream6_Handler;

   ---------------------------
   -- Flush_In_Failure_Mode --
   ---------------------------

   procedure Flush_In_Failure_Mode is
   begin
      null;
   end Flush_In_Failure_Mode;

   ----------------------
   -- Get_Asynchronous --
   ----------------------

   procedure Get_Asynchronous
     (Item    : out Character;
      Success : in out Boolean) is
   begin
      raise Program_Error;
   end Get_Asynchronous;

   ---------------------
   -- Get_Synchronous --
   ---------------------

   procedure Get_Synchronous (Item : out Character) is
   begin
      loop
         exit when Receive_Head /= Receive_Tail;

         Ada.Synchronous_Task_Control.Suspend_Until_True (Lock);
      end loop;

      Receive_Tail :=
        (if Receive_Tail = Receive_Buffer'Last
           then Receive_Buffer'First
           else @ + 1);
      Item := Character'Val (Receive_Buffer (Receive_Tail));
   end Get_Synchronous;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      RCC_Periph.AHB1ENR.GPIOAEN := True;
      RCC_Periph.APB2ENR.USART6EN := True;
      RCC_Periph.AHB1ENR.DMA2EN := True;

      TX_Line.Configure_Alternative_Function
        (Line  => A0B.STM32F401.USART_Function_Lines.USART6_TX,
         Mode  => A0B.STM32F401.GPIO.Push_Pull,
         Speed => A0B.STM32F401.GPIO.Very_High,
         Pull  => A0B.STM32F401.GPIO.No);
      RX_Line.Configure_Alternative_Function
        (Line  => A0B.STM32F401.USART_Function_Lines.USART6_RX,
         Mode  => A0B.STM32F401.GPIO.Push_Pull,
         Speed => A0B.STM32F401.GPIO.Very_High,
         Pull  => A0B.STM32F401.GPIO.No);

      USART6_Periph.CR1 :=
        (SBK    => False,  --  No break character is transmitted
         RWU    => False,  --  Receiver in active mode
         RE     => True,
         --  Receiver is enabled and begins searching for a start bit
         TE     => True,   --  Transmitter is enabled
         IDLEIE => False,  --  Interrupt is inhibited
         RXNEIE => True,
         --  An USART interrupt is generated whenever ORE=1 or RXNE=1
         TCIE   => False,  --  Interrupt is inhibited
         TXEIE  => False,  --  Interrupt is inhibited
         PEIE   => False,  --  Interrupt is inhibited
         PS     => <>,     --  Parity check is disabled, meaningless
         PCE    => False,  --  Parity control disabled
         WAKE   => False,  --  XXX ???
         M      => False,  --  1 Start bit, 8 Data bits, n Stop bit
         UE     => False,  --  USART prescaler and outputs disabled
                           --  Disable to be able to configure other registers
         OVER8  => False,  --  oversampling by 16
         others => <>);

      USART6_Periph.CR2 :=
        (ADD    => <>,     --  Not used
         LBDL   => <>,     --  Not used
         LBDIE  => <>,     --  Not used
         LBCL   => <>,     --  Not used
         CPHA   => <>,     --  Not used
         CPOL   => <>,     --  Not used
         CLKEN  => False,  --  CK pin disabled
         STOP   => 2#00#,  --  1 Stop bit
         LINEN  => False,  --  LIN mode disabled
         others => <>);

      USART6_Periph.CR3 :=
        (EIE    => False,  --  Error interrupt enable
         IREN   => False,  --  IrDA disabled
         IRLP   => <>,     --  Not used
         HDSEL  => <>,     --  Not used
         NACK   => <>,     --  Not used
         SCEN   => False,  --  Smartcard Mode disabled
         DMAR   => False,  --  DMA mode is disabled for reception
         DMAT   => True,   --  DMA mode is enabled for transmission
         RTSE   => False,  --  RTS hardware flow control disabled
         CTSE   => False,  --  CTS hardware flow control disabled
         CTSIE  => False,  --  Interrupt is inhibited
         ONEBIT => False,  --  Three sample bit method
         others => <>);

      USART6_Periph.BRR :=
        (DIV_Fraction => 10,  --  115_200 when APB2 @84_000_000 MHz
         DIV_Mantissa => 45,
         others       => <>);

      --  Configure DMA stream (DMA 2 Stream 6 Channel 5)

      Transmit_Stream.Configure_Memory_To_Peripheral
        (Channel    => 5,
         Peripheral => USART6_Periph.DR'Address);
      Transmit_Stream.Enable_Transfer_Complete_Interrupt;
      Transmit_Stream.Set_Interrupt_Callback
        (On_DMA_Interrupt_Callbacks.Create_Callback);

      --  Enable USART

      USART6_Periph.CR1.UE := True;

      A0B.ARMv7M.NVIC_Utilities.Clear_Pending (A0B.STM32F401.USART6);
      A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (A0B.STM32F401.USART6);

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

   ---------
   -- Put --
   ---------

   procedure Put (Item : String) is
   begin
      for C of Item loop
         Transmit_Head :=
           (if Transmit_Head = Transmit_Buffer'Last
              then Transmit_Buffer'First
              else @ + 1);
         Transmit_Buffer (Transmit_Head) := Character'Pos (C);
      end loop;

      --  Enable TC interrupt, operation will be completed in the interrupt
      --  handler.

      USART6_Periph.CR1.TCIE := True;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Item : String) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;

   --------------------
   -- USART6_Handler --
   --------------------

   procedure USART6_Handler is
      Mask  : constant CR1_Register := USART6_Periph.CR1;
      State : constant SR_Register  := USART6_Periph.SR;

   begin
      if State.TXE and Mask.TXEIE then
         --  if Transmit_Head /= Transmit_Tail then
         --     USART6_Periph.DR.DR :=
         --       DR_DR_Field (Transmit_Buffer (Transmit_Tail));
         --     Transmit_Tail := @ + 1;
         --
         --  else
         --     USART6_Periph.CR1.TXEIE := False;
         --  end if;

         raise Program_Error;
      end if;

      if State.TC and Mask.TCIE then
         if Transmit_Tail = Transmit_Head then
            --  No more data, disable TC interrupt

            USART6_Periph.CR1.TCIE := False;

         else
            Transmit_Tail :=
              (if Transmit_Tail = Transmit_Buffer'Last
                 then Transmit_Buffer'First
                 else @ + 1);

            if Transmit_Tail <= Transmit_Head then
               --  Transmit data between tail and head

               Transmit_Stream.Set_Memory_Buffer
                 (Transmit_Buffer (Transmit_Tail)'Address,
                  Interfaces.Unsigned_16 (Transmit_Head - Transmit_Tail + 1));
               Transmit_Tail := Transmit_Head;

            else
               --  Transmit data from tail till end of the buffer memory,
               --  remaining data will be transmitted by next operation.

               Transmit_Stream.Set_Memory_Buffer
                 (Transmit_Buffer (Transmit_Tail)'Address,
                  Interfaces.Unsigned_16
                    (Transmit_Buffer'Last - Transmit_Tail + 1));
               Transmit_Tail := Transmit_Buffer'Last;
            end if;

            USART6_Periph.SR.TC := False;
            Transmit_Stream.Enable;
         end if;
      end if;

      if State.RXNE and Mask.RXNEIE then
         Receive_Head :=
           (if Receive_Head = Receive_Buffer'Last
              then Receive_Buffer'First
              else @ + 1);

         Receive_Buffer (Receive_Head) :=
           Interfaces.Unsigned_8 (USART6_Periph.DR.DR);

         Ada.Synchronous_Task_Control.Set_True (Lock);
      end if;
   end USART6_Handler;

end Hexapod.Console;
