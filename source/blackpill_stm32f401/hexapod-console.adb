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

with Interfaces;
with System.Storage_Elements;

with A0B.ARMv7M.NVIC_Utilities;
with A0B.STM32F401.GPIO.PIOA;
with A0B.STM32F401.SVD.DMA;   use A0B.STM32F401.SVD.DMA;
with A0B.STM32F401.SVD.RCC;   use A0B.STM32F401.SVD.RCC;
with A0B.STM32F401.SVD.USART; use A0B.STM32F401.SVD.USART;
with A0B.Types;

package body Hexapod.Console is

   use type A0B.Types.Unsigned_32;

   TX_Line : A0B.STM32F401.GPIO.GPIO_Line renames A0B.STM32F401.GPIO.PIOA.PA11;
   RX_Line : A0B.STM32F401.GPIO.GPIO_Line renames A0B.STM32F401.GPIO.PIOA.PA12;

   type Unsigned_8_Array is
     array (A0B.Types.Unsigned_32 range <>) of A0B.Types.Unsigned_8;

   procedure USART6_Handler
     with Export, Convention => C, External_Name => "USART6_Handler";

   --  procedure DMA2_Stream1_Handler is null
   --    with Export, Convention => C, External_Name => "DMA2_Stream1_Handler";

   procedure DMA2_Stream6_Handler
     with Export, Convention => C, External_Name => "DMA2_Stream6_Handler";

   Logo : constant String :=
     ASCII.CR & ASCII.LF
     & "          /\ .---._" & ASCII.CR & ASCII.LF
     & "       /\/.-. /\ /\/\" & ASCII.CR & ASCII.LF
     & "     //\\oo //\\/\\\\" & ASCII.CR & ASCII.LF
     & "    //  /""/`---\\ \\""`-._" & ASCII.CR & ASCII.LF
     & "_.-'""           ""`-.`-." & ASCII.CR & ASCII.LF
     & ASCII.CR & ASCII.LF;

   --  UART : BBF.Board.UART.UART_Driver
   --           (Receive_Queue => 0, Transmit_Queue => 255);

   Transmit_Buffer : Unsigned_8_Array (0 .. 255);
   Transmit_Head   : A0B.Types.Unsigned_32 := 0;
   Transmit_Tail   : A0B.Types.Unsigned_32 := 0;
   --  Receive_Buffer  : Unsigned_8_Array (0 .. 255);
   --  Receive_Head    : A0B.Types.Unsigned_32 := 0;
   --  Receive_Tail    : A0B.Types.Unsigned_32 := 0;

   --------------------------
   -- DMA2_Stream6_Handler --
   --------------------------

   procedure DMA2_Stream6_Handler is
      Mask  : constant S6CR_Register := DMA2_Periph.S6CR;
      State : constant HISR_Register := DMA2_Periph.HISR;

   begin
      if State.TCIF6 and Mask.TCIE then
         --  DMA2_Periph.HIFCR.CFEIF6  := True;
         --  DMA2_Periph.HIFCR.CDMEIF6 := True;
         --  DMA2_Periph.HIFCR.CTEIF6  := True;
         --  DMA2_Periph.HIFCR.CHTIF6  := True;
         DMA2_Periph.HIFCR.CTCIF6 := True;

         DMA2_Periph.S6CR.EN := False;

      else
         raise Program_Error;
      end if;
   end DMA2_Stream6_Handler;

   ----------------------
   -- Get_Asynchronous --
   ----------------------

   procedure Get_Asynchronous
     (Item    : out Character;
      Success : in out Boolean)
   is
   --     use type BBF.Unsigned_16;
   --
   --     Buffer : BBF.Unsigned_8_Array_16 (0 ..0);
   --     Size   : BBF.Unsigned_16;

   begin
      null;
   --     if not Success then
   --        Item := Character'Val (0);
   --
   --        return;
   --     end if;
   --
   --     UART.Receive_Asynchronous (Buffer, Size);
   --
   --     if Size = 0 then
   --        Success := False;
   --        Item    := Character'Val (0);
   --
   --     else
   --        Success := True;
   --        Item    := Character'Val (Buffer (0));
   --        --  Received := False;
   --     end if;
   end Get_Asynchronous;

   ---------------------
   -- Get_Synchronous --
   ---------------------

   procedure Get_Synchronous (Item : out Character) is
   --     use type BBF.Unsigned_16;
   --
   --     Buffer : BBF.Unsigned_8_Array_16 (0 ..0);
   --     Size   : BBF.Unsigned_16;

   begin
      null;
   --     loop
   --        UART.Receive_Asynchronous (Buffer, Size);
   --
   --        if Size /= 0 then
   --           Item := Character'Val (Buffer (0));
   --
   --           return;
   --        end if;
   --     end loop;
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
        (Line  => A0B.STM32F401.USART6_TX,
         Mode  => A0B.STM32F401.GPIO.Push_Pull,
         Speed => A0B.STM32F401.GPIO.Very_High,
         Pull  => A0B.STM32F401.GPIO.No);
      RX_Line.Configure_Alternative_Function
        (Line  => A0B.STM32F401.USART6_RX,
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

      DMA2_Periph.S6CR :=
        (EN     => False,   --  Stream disabled
         DMEIE  => False,   --  DME interrupt disabled
         TEIE   => False,   --  TE interrupt disabled
         HTIE   => False,   --  HT interrupt disabled
         TCIE   => True,    --  TC interrupt enabled
         PFCTRL => False,   --  The DMA is the flow controller
         DIR    => 2#01#,   --  Memory-to-peripheral
         CIRC   => False,   --  Circular mode disabled
         PINC   => False,   --  Peripheral address pointer is fixed
         MINC   => True,
         --  Memory address pointer is incremented after each data transfer
         --  (increment is done according to MSIZE)
         PSIZE  => 2#00#,   --  Byte (8-bit)
         MSIZE  => 2#00#,   --  Byte (8-bit)
         PINCOS => <>,      --  No meaning
         PL     => 2#00#,   --  Low
         DBM    => False,   --  No buffer switching at the end of transfer
         CT     => False,
         --  The current target memory is Memory 0 (addressed by the
         --  DMA_SxM0AR pointer)
         ACK    => <>,      --  ??? Not documented
         PBURST => 2#00#,   --  single transfer
         MBURST => 2#00#,   --  single transfer
         CHSEL  => 2#101#,  --  channel 5 selected
         others => <>);
      DMA2_Periph.S6PAR :=
        Interfaces.Unsigned_32
          (System.Storage_Elements.To_Integer (USART6_Periph.DR'Address));

      A0B.ARMv7M.NVIC_Utilities.Clear_Pending (A0B.STM32F401.DMA2_Stream6);
      A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (A0B.STM32F401.DMA2_Stream6);

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
   begin
      for C of Item loop
         Transmit_Head := @ + 1;
         Transmit_Head :=
           (if Transmit_Head > Transmit_Buffer'Last
              then Transmit_Buffer'First else @);

         Transmit_Buffer (Transmit_Head) := Character'Pos (C);
      end loop;

      --  USART6_Periph.CR1.TXEIE := True;
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
            USART6_Periph.CR3.DMAT := False;
            USART6_Periph.CR1.TCIE := False;

         else
            if Transmit_Tail < Transmit_Head then
               DMA2_Periph.S6NDTR.NDT :=
                 S6NDTR_NDT_Field (Transmit_Head - Transmit_Tail);
               DMA2_Periph.S6M0AR :=
                 Interfaces.Unsigned_32
                   (System.Storage_Elements.To_Integer
                      (Transmit_Buffer (Transmit_Tail)'Address));
               Transmit_Tail := Transmit_Head;

            else
               DMA2_Periph.S6NDTR.NDT :=
                 S6NDTR_NDT_Field (Transmit_Buffer'Last - Transmit_Tail + 1);
               DMA2_Periph.S6M0AR :=
                 Interfaces.Unsigned_32
                   (System.Storage_Elements.To_Integer
                      (Transmit_Buffer (Transmit_Tail)'Address));
               Transmit_Tail := Transmit_Buffer'First;
            end if;

            USART6_Periph.SR.TC := False;
            DMA2_Periph.S6CR.EN := True;
         end if;
      end if;

      if State.RXNE and Mask.RXNEIE then
         raise Program_Error;
      end if;
   end USART6_Handler;

end Hexapod.Console;
