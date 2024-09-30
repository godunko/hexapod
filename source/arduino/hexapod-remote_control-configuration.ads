--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Hardware specific configuration for Arduino Due (ATSAM3X8E) board.

with A0B.ATSAM3X8E.PIO.PIOB;
with A0B.ATSAM3X8E.USART.SPI;

private package Hexapod.Remote_Control.Configuration is

   SPI_Device : aliased A0B.ATSAM3X8E.USART.SPI_Slave_Device
     (A0B.ATSAM3X8E.USART.SPI.USART1_SPI'Access);
   ACK_Pin    : A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class
     renames A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class
       (A0B.ATSAM3X8E.PIO.PIOB.PB26);

   procedure Initialize;
   --  Initialize and configure MCU's peripherals.

end Hexapod.Remote_Control.Configuration;
